#![feature(concat_idents)]
pub use acmd_proc_macros::*;

use smash::lua2cpp::{L2CAgentBase, L2CFighterCommon, L2CFighterBase};
use smash::phx::Hash40;

type Callback = fn(&mut L2CFighterCommon);
type WeaponCallback = fn(&mut L2CFighterBase);
type Predicate = unsafe fn(&mut L2CAgentBase, Hash40) -> bool;

#[macro_export] macro_rules! add_hooks {
    ($($hook:ident),* $(,)?) => {
        $(
            $crate::add_hook!($hook);
        )*
    };
}

#[macro_export] macro_rules! add_custom_hooks {
    ($($hook:ident),* $(,)?) => {
        $(
            unsafe {
                acmd::add_acmd_load_hook($hook, |_,_| false);
            }
        )*
    };
}

#[macro_export] macro_rules! add_custom_weapon_hooks {
    ($($hook:ident),* $(,)?) => {
        $(
            unsafe {
                acmd::add_acmd_load_weapon_hook($hook, |_,_| false);
            }
        )*
    };
}

extern "Rust" {
    pub fn add_acmd_load_hook(callback: Callback, predicate: Predicate);
    pub fn add_acmd_load_weapon_hook(callback: WeaponCallback, predicate: Predicate);
}
