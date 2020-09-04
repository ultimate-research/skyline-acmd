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
            target_frame = #arg as f32;
        )
    } else if func_call.name.is_ident("wait") {
        //wait
        let arg = &func_call.args.iter().nth(0).expect("Missing argument in wait call").expr;
        quote!(
            target_frame += #arg as f32;
        )
    } else if func_call.name.get_ident().is_some() {
        // ACMD functions
        let func_name = &func_call.name;
        let args = func_call.args.iter().map(|arg| arg.expr.clone());

        if func_name.to_token_stream().to_string() == "FT_MOTION_RATE" {
            return quote!(
                if current_frame >= target_frame {
                    ::smash::app::lua_bind::MotionModule::set_rate(
                        module_accessor,
                        #(
                            (1.0 / #args as f32).into()
                        ),*
                    );
                }
            );
        }
        else if func_name.to_token_stream().to_string() == "game_CaptureCutCommon" {
            return quote!(
                if current_frame >= target_frame {
                    l2c_agent.clear_lua_stack();
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(*::smash::lib::lua_const::FIGHTER_ATTACK_ABSOLUTE_KIND_CATCH as u64));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(0 as u64));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_num(3.0));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(100 as u64));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(0 as u64));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(60 as u64));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_num(0.0));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_num(1.0));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(*::smash::lib::lua_const::ATTACK_LR_CHECK_F as u64));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_num(0.0));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_bool(true));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(::smash::hash40("collision_attr_normal")));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(*::smash::lib::lua_const::ATTACK_SOUND_LEVEL_S as u64));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(*::smash::lib::lua_const::COLLISION_SOUND_ATTR_KICK as u64));
                    l2c_agent.push_lua_stack(&mut ::smash::lib::L2CValue::new_int(*::smash::lib::lua_const::ATTACK_REGION_NONE as u64));
                    ::smash::app::sv_animcmd::ATTACK_ABS(lua_state);
                }
            );
        }

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
            ::smash::app::lua_bind::#func_name(
                module_accessor,
                #(
                    (#args).into()
                ),*
            );
        )
    }
}

const LAST_FRAME_GLOBAL : &str = "last_excute_frame";

#[proc_macro]
pub fn generate_acmd_is_execute(input: TokenStream) -> TokenStream {
    let expr = syn::parse_macro_input!(input as Expr);

    if let Expr::Path(path) = expr {
        let path = path.path;
        if path.is_ident("is_execute") || path.is_ident("is_excute") {
            return quote!(
                if last_excute_frame > current_frame || true_frame_1 {
                    last_excute_frame = -1.0;
                }

                let #path = current_frame >= target_frame && last_excute_frame < target_frame;

                if #path {
                    last_excute_frame = target_frame;
                }

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
    syn::custom_keyword!(Iterations);
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

struct AcmdFor {
    pub for_token: Token![for],
    pub parens: syn::token::Paren,
    pub iter_count: Expr,
    pub iter_keyword: kw::Iterations,
    pub block: AcmdBlock
}

impl Parse for AcmdFor {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        Ok(Self {
            for_token: input.parse()?,
            parens: syn::parenthesized!(content in input),
            iter_count: content.parse()?,
            iter_keyword: content.parse()?,
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
    For(AcmdFor),
    FuncCall(AcmdFuncCall),
    RustBlock(InlineRustBlock),
}

impl Parse for AcmdStatement {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(Token![if]) {
            Ok(Self::If(input.parse()?))
        } else if lookahead.peek(Token![for]) {
            Ok(Self::For(input.parse()?))
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
            Self::For(acmd_for) => {
                let iter_count = &acmd_for.iter_count;
                let acmd_block = &acmd_for.block;
                quote!(
                    for _ in (0..#iter_count) {
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
            let mut target_frame = 1.0;
            let mut current_frame = ::smash::app::lua_bind::MotionModule::frame(module_accessor) + 2.0;
            let globals = fighter.globals_mut();
            static mut last_excute_frame: f32 = -1.0;

            // MotionModule::frame(module_accessor) returns 0.0 the first 2 frames
            let true_frame_1 = match globals.val_type {
                smash::lib::L2CValueType::Table => {
                    false
                }
                _ => {
                    current_frame = 1.0;
                    true
                }
            };
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

    let animcmd_str = _animcmd
        .to_token_stream()
        .to_string();

    let animcmd_split = animcmd_str
        .split("_")
        .collect::<Vec<&str>>();

    let last_frame_global_str = match animcmd_split.first() {
        Some(str) => format!("{}_{}", &str[1..], LAST_FRAME_GLOBAL), // remove quote
        None => LAST_FRAME_GLOBAL.to_string()
    };

    let _orig_fn = mod_fn.block.to_token_stream();
    let _orig_fn_name = &mod_fn.sig.ident;

    let last_frame_global_defn: Stmt = parse_quote! {
        let last_frame_global = #last_frame_global_str;
    };

    let lua_state_defn: Stmt = parse_quote! {
        let lua_state = fighter.lua_state_agent;
    };

    let module_accessor_defn: Stmt = parse_quote! {
        let module_accessor = unsafe { smash::app::sv_system::battle_object_module_accessor(lua_state) };
    };

    let conditional_wrap: Stmt = parse_quote! {
        unsafe {
            if smash::app::utility::get_category(module_accessor) == #_category
                && (smash::app::utility::get_kind(module_accessor) == #_kind || *#_kind == *smash::lib::lua_const::FIGHTER_KIND_ALL) {
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
    mod_fn.block.stmts.push(last_frame_global_defn);
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
                (smash::app::utility::get_kind(module_accessor) == #_kind || *#_kind == *smash::lib::lua_const::FIGHTER_KIND_ALL);
        }
    ).to_tokens(&mut output);

    let _add_hook_fn = quote::format_ident!(
        "{}_skyline_acmd_internal_add_hook_fn",
        _orig_fn_name
    );

    if _category.to_token_stream().to_string() == "BATTLE_OBJECT_CATEGORY_FIGHTER" {
        quote!(
            #[allow(non_upper_case_globals)]
            pub unsafe fn #_add_hook_fn() {
                acmd::add_acmd_load_hook(#_orig_fn_name, #_pred_fn);
            }
        ).to_tokens(&mut output);
    } else {
        quote!(
            #[allow(non_upper_case_globals)]
            pub unsafe fn #_add_hook_fn()  {
                acmd::add_acmd_load_weapon_hook(#_orig_fn_name, #_pred_fn);
            }
        ).to_tokens(&mut output);
    }

    output.into()
}

#[proc_macro]
pub fn add_hook(input: TokenStream) -> TokenStream {
    let ident = syn::parse_macro_input!(input as Ident);

    let add_hook_fn = quote::format_ident!("{}_skyline_acmd_internal_add_hook_fn", ident);

    quote!(
        unsafe {
            #add_hook_fn();
        }
    ).into()
}
