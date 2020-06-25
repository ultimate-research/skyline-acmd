#![feature(concat_idents)]
pub use acmd_proc_macros::*;

use smash::lua2cpp::L2CFighterCommon;
use smash::lua2cpp::L2CAgentBase;
use smash::phx::Hash40;

extern "Rust" {
    pub fn add_acmd_load_hook(callback: fn(&mut L2CFighterCommon), predicate: unsafe fn(&mut L2CAgentBase, Hash40) -> bool);
}
