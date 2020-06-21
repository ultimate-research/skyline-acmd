use smash::lua2cpp::L2CFighterCommon;

pub fn add_hook(callback: fn(&mut L2CFighterCommon)) {
    unsafe {
        add_acmd_load_hook(callback);
    }
}

extern "Rust" {
    fn add_acmd_load_hook(callback: fn(&mut L2CFighterCommon));
}
