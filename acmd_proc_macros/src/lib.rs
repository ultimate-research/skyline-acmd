use proc_macro::TokenStream;
use syn::{Ident, Path, Expr, Token};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use quote::quote;

#[derive(Debug)]
struct FuncCall {
    pub name: Path,
    pub paren_token: syn::token::Paren,
    pub args: Punctuated<ArgExpr, Token![,]>,
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

impl Parse for FuncCall {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            name: input.parse()?,
            paren_token: syn::parenthesized!(content in input),
            args: content.parse_terminated(ArgExpr::parse)?,
        })
    }
}

#[proc_macro]
pub fn single_acmd_func(input: TokenStream) -> TokenStream {
    let func_call = syn::parse_macro_input!(input as FuncCall);

    if func_call.name.is_ident("frame") {
        // frame
        todo!()
    } else if func_call.name.is_ident("wait") {
        //wait
        todo!()
    } else if func_call.name.get_ident().is_some() {
        // ACMD functions
        let func_name = func_call.name;
        let args = func_call.args.iter().map(|arg| arg.expr.clone());
        quote!(
            l2c_agent.clear_lua_stack();
            #(
                l2c_agent.push_lua_stack(&mut (#args).into());
            )*
            ::smash::app::sv_animcmd::#func_name(lua_state);
        ).into()
    } else if func_call.name.segments.iter().next().unwrap().ident
                    .to_string().starts_with("sv_") {
        // Lua calling convention
        let func_name = func_call.name;
        let args = func_call.args.iter().map(|arg| arg.expr.clone());
        quote!(
            l2c_agent.clear_lua_stack();
            #(
                l2c_agent.push_lua_stack(&mut (#args).into());
            )*
            ::smash::app::#func_name(lua_state);
        ).into()
    } else {
        // Module functions
        todo!()
    }
}
