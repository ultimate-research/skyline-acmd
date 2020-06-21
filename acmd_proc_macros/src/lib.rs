use proc_macro::TokenStream;
use syn::{Ident, Path, Expr, Token};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use quote::{quote, ToTokens, TokenStreamExt};
use proc_macro2::TokenStream as TokenStream2;

#[derive(Debug)]
struct AcmdFuncCall {
    pub name: Path,
    pub paren_token: syn::token::Paren,
    pub args: Punctuated<ArgExpr, Token![,]>,
    pub semi: Option<Token![;]>,
}

#[derive(Debug)]
struct ArgExpr {
    pub name: Option<Ident>,
    pub expr: Expr,
}

impl Parse for ArgExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Ident) && input.peek2(Token![=]) {
            let name = input.parse()?;
            let _: Token![=] = input.parse()?;
            Ok(Self {
                name: Some(name),
                expr: input.parse()?
            })
        } else {
            Ok(Self {
                name: None,
                expr: input.parse()?
            })
        }
    }
}

impl Parse for AcmdFuncCall {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            name: input.parse()?,
            paren_token: syn::parenthesized!(content in input),
            args: content.parse_terminated(ArgExpr::parse)?,
            semi: if input.peek(Token![;]) {
                Some(input.parse()?)
            } else {
                None
            }
        })
    }
}

fn single_acmd_func(func_call: &AcmdFuncCall) -> TokenStream2 {
    if func_call.name.is_ident("frame") {
        // frame
        let arg = &func_call.args.iter().nth(0).expect("Missing argument in frame call").expr;
        quote!(
            target_frame = #arg;
            if target_frame > current_frame {
                return;
            }
        )
    } else if func_call.name.is_ident("wait") {
        //wait
        let arg = &func_call.args.iter().nth(0).expect("Missing argument in wait call").expr;
        quote!(
            target_frame += #arg;
            if target_frame > current_frame {
                return;
            }
        )
    } else if func_call.name.get_ident().is_some() {
        // ACMD functions
        let func_name = &func_call.name;
        let args = func_call.args.iter().map(|arg| arg.expr.clone());
        quote!(
            l2c_agent.clear_lua_stack();
            #(
                l2c_agent.push_lua_stack(&mut (#args).into());
            )*
            ::smash::app::sv_animcmd::#func_name(lua_state);
        )
    } else if func_call.name.segments.iter().next().unwrap().ident
                    .to_string().starts_with("sv_") {
        // Lua calling convention
        let func_name = &func_call.name;
        let args = func_call.args.iter().map(|arg| arg.expr.clone());
        quote!(
            l2c_agent.clear_lua_stack();
            #(
                l2c_agent.push_lua_stack(&mut (#args).into());
            )*
            ::smash::app::#func_name(lua_state);
        )
    } else {
        // Module functions
        let func_name = &func_call.name;
        let args = func_call.args.iter().map(|arg| arg.expr.clone());
        quote!(
            ::smash::app::lua_bind::#func_name(module_accessor, #(#args),*);
        )
    }
}


#[proc_macro]
pub fn generate_acmd_is_execute(input: TokenStream) -> TokenStream {
    let expr = syn::parse_macro_input!(input as Expr);

    if let Expr::Path(path) = expr {
        let path = path.path;
        if path.is_ident("is_execute") {
            return quote!(
                let #path = target_frame == current_frame;
            ).into();
        }
    }

    quote!(
        
    ).into()
}

mod kw {
    syn::custom_keyword!(rust);
}

struct AcmdBlock {
    pub braces: syn::token::Brace,
    pub statements: Vec<AcmdStatement>
}

impl Parse for AcmdBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            braces: syn::braced!(content in input),
            statements: {
                let mut items = Vec::new();
                while !content.is_empty() {
                    items.push(content.parse()?);
                }
                items
            }
        })
    }
}

impl ToTokens for AcmdBlock {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.append_all(self.statements.iter())
    }
}

struct AcmdIf {
    pub if_token: Token![if],
    pub parens: syn::token::Paren,
    pub cond: Expr,
    pub block: AcmdBlock
}

impl Parse for AcmdIf {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            if_token: input.parse()?,
            parens: syn::parenthesized!(content in input),
            cond: content.parse()?,
            block: input.parse()?
        })
    }
}

struct InlineRustBlock {
    pub rust_token: kw::rust,
    pub block: syn::Block
}

impl Parse for InlineRustBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            rust_token: input.parse()?,
            block: input.parse()?
        })
    }
}

enum AcmdStatement {
    If(AcmdIf),
    FuncCall(AcmdFuncCall),
    RustBlock(InlineRustBlock),
}

impl Parse for AcmdStatement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![if]) {
            Ok(Self::If(input.parse()?))
        } else if lookahead.peek(kw::rust) {
            Ok(Self::RustBlock(input.parse()?))
        } else {
            Ok(Self::FuncCall(input.parse()?))
        }
    }
}

impl ToTokens for AcmdStatement {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let new_tokens = match self {
            Self::If(acmd_if) => {
                let cond = &acmd_if.cond;
                let acmd_block = &acmd_if.block;
                quote!(
                    ::acmd::generate_acmd_is_execute!(#cond);
                    if #cond {
                        #acmd_block
                    }
                )
            }
            Self::FuncCall(func_call) => {
                single_acmd_func(func_call)
            }
            Self::RustBlock(rust_block) => {
                let stmts = rust_block.block.stmts.iter();
                quote!(
                    #(
                        #stmts
                    )*
                )
            }
        };
        tokens.append_all([new_tokens].iter());
    }
}

struct AcmdInput {
    pub l2c_state: Option<Expr>,
    pub acmd: AcmdBlock
}

impl Parse for AcmdInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek2(Token![,]) {
            let l2c_state = Some(input.parse()?);
            let _: Token![,] = input.parse()?;
            Ok(Self {
                l2c_state,
                acmd: input.parse()?
            })
        } else {
            Ok(Self {
                l2c_state: None,
                acmd: input.parse()?
            })
        }
    }
}

#[proc_macro]
pub fn acmd(input: TokenStream) -> TokenStream {
    let acmd_input = syn::parse_macro_input!(input as AcmdInput);

    let setup = acmd_input.l2c_state.map(|l2c_state|{
        quote!(
            let l2c_agent = &mut ::smash::lib::L2CAgent::new(#l2c_state);
            let lua_state = #l2c_state;
            let module_accessor = ::smash::app::sv_system::battle_object_module_accessor(lua_state);
            let mut target_frame = 0;
            let current_frame = ::smash::app::lua_bind::MotionModule::frame(module_accessor).round() as i32;
        )
    });

    let acmd_stmts = acmd_input.acmd.statements;

    quote!(
        #setup

        #(
            #acmd_stmts
        )*
    ).into()
}
