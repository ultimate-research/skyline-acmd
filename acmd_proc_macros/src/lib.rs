use proc_macro::TokenStream;
use syn::{Ident, Path, Expr, token, Token, Stmt, parse_quote, parse_macro_input, parenthesized};
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
        if path.is_ident("is_execute") || path.is_ident("is_excute") {
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
    syn::custom_keyword!(name);
    syn::custom_keyword!(battle_object_category);
    syn::custom_keyword!(battle_object_kind);
    syn::custom_keyword!(animation);
    syn::custom_keyword!(animcmd);
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

#[derive(Default)]
struct AcmdAttrs {
    pub battle_object_category: Option<syn::Path>,
    pub battle_object_kind: Option<syn::Path>,
    pub animation: Option<syn::LitStr>,
    pub animcmd: Option<syn::LitStr>
}

fn merge(attr1: AcmdAttrs, attr2: AcmdAttrs) -> AcmdAttrs {
    let (
        AcmdAttrs { battle_object_category: c1, battle_object_kind: k1, animation: a1, animcmd: ac1},
        AcmdAttrs { battle_object_category: c2, battle_object_kind: k2, animation: a2, animcmd: ac2},
    ) = (attr1, attr2);


    AcmdAttrs {
        battle_object_category: c1.or(c2),
        battle_object_kind: k1.or(k2),
        animation: a1.or(a2),
        animcmd: ac1.or(ac2)
    }
}

impl Parse for AcmdAttrs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let look = input.lookahead1();
        let attr = if look.peek(kw::battle_object_category) {
            let MetaItem::<kw::battle_object_category, syn::Path> { item: cat, .. } = input.parse()?;
            
            let mut a = AcmdAttrs::default();
            a.battle_object_category = Some(cat);
            a
        } else if look.peek(kw::battle_object_kind) {
            let MetaItem::<kw::battle_object_kind, syn::Path> { item: kind, .. } = input.parse()?;
            
            let mut a = AcmdAttrs::default();
            a.battle_object_kind = Some(kind);
            a
        } else if look.peek(kw::animation) {
            let MetaItem::<kw::animation, syn::LitStr> { item: anim, .. } = input.parse()?;
            
            let mut a = AcmdAttrs::default();
            a.animation = Some(anim);
            a
        } else if look.peek(kw::animcmd) {
            let MetaItem::<kw::animcmd, syn::LitStr> { item: animcmd, .. } = input.parse()?;
            
            let mut a = AcmdAttrs::default();
            a.animcmd = Some(animcmd);
            a
        } else {
            return Err(look.error());
        };

        Ok(if input.peek(Token![,]) {
            let _: Token![,] = input.parse()?;
            if input.is_empty() {
                attr
            } else {
                merge(attr, input.parse()?)
            }
        } else {
            attr
        })
    }
}


#[derive(Debug, Clone)]
struct MetaItem<Keyword: Parse, Item: Parse> {
    pub ident: Keyword,
    pub item: Item,
}

impl<Keyword: Parse, Item: Parse> Parse for MetaItem<Keyword, Item> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse()?;
        let item = if input.peek(token::Paren) {
            let content;
            parenthesized!(content in input);
            content.parse()?
        } else {
            input.parse::<Token![=]>()?;
            input.parse()?
        };

        Ok(Self {
            ident,
            item
        })
    }
}

#[proc_macro_attribute]
pub fn acmd_func(attrs: TokenStream, input: TokenStream) -> TokenStream {
    let mut mod_fn = parse_macro_input!(input as syn::ItemFn);
    let attrs = parse_macro_input!(attrs as AcmdAttrs);
    let mut output = TokenStream2::new();

    let _category = attrs.battle_object_category.unwrap();
    let _kind = attrs.battle_object_kind.unwrap();
    let _animation = attrs.animation.unwrap();
    let _animcmd = attrs.animcmd.unwrap();

    let _orig_fn = mod_fn.block.to_token_stream();

    let lua_state_defn: Stmt = parse_quote! {
        let lua_state = fighter.lua_state_agent;
    };

    let module_accessor_defn: Stmt = parse_quote! {
        let module_accessor = unsafe { smash::app::sv_system::battle_object_module_accessor(lua_state) }; 
    };

    let conditional_wrap: Stmt = parse_quote! {
        unsafe {
            if smash::app::utility::get_category(module_accessor) == #_category 
                && smash::app::utility::get_kind(module_accessor) == #_kind {
                if smash::app::lua_bind::MotionModule::motion_kind(module_accessor) == smash::hash40(#_animation) {
                    use ::acmd::acmd;
                    ::acmd::acmd!(lua_state, {
                        rust {
                            #_orig_fn
                        }
                    });
                }
            }
        }
    };

    mod_fn.block.stmts.clear();
    mod_fn.block.stmts.push(lua_state_defn);
    mod_fn.block.stmts.push(module_accessor_defn);
    mod_fn.block.stmts.push(conditional_wrap);

    mod_fn.to_tokens(&mut output);

    let _pred_fn = quote::format_ident!(
        "{}_skyline_acmd_internal_predicate_fn",
        mod_fn.sig.ident
    );

    quote!(        
        #[allow(non_upper_case_globals)]
        pub unsafe fn #_pred_fn(agent: &mut smash::lua2cpp::L2CAgentBase, hash: smash::phx::Hash40) -> bool {
            let module_accessor = smash::app::sv_system::battle_object_module_accessor(agent.lua_state_agent);
            return 
                hash.hash == smash::hash40(#_animcmd) &&
                smash::app::utility::get_category(module_accessor) == #_category &&
                smash::app::utility::get_kind(module_accessor) == #_kind;
        }
    ).to_tokens(&mut output);

    output.into()
}

#[proc_macro]
pub fn add_hook(input: TokenStream) -> TokenStream {
    let ident = syn::parse_macro_input!(input as Ident);

    let pred_fn = quote::format_ident!("{}_skyline_acmd_internal_predicate_fn", ident);

    quote!(
        unsafe {
            acmd::add_acmd_load_hook(#ident, #pred_fn);
        }
    ).into()
}
