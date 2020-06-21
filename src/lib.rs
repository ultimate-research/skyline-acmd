pub use acmd_proc_macros::*;

#[macro_export] macro_rules! acmd {
    (
        $l2c_state:expr,
        {
            $(
                $($func_name:ident)::*(
                    $($args:tt)*
                );
            )*
        }
    ) => {
        let l2c_agent = &mut L2CAgent::new($l2c_state);
        let lua_state = $l2c_state;

        $(
            acmd::single_acmd_func!($($func_name)::* ($($args)*));
        )*
    };
}

use smash::lua2cpp::L2CFighterCommon;

pub fn add_hook(callback: fn(&mut L2CFighterCommon)) {
    unsafe {
        add_acmd_load_hook(callback);
    }
}

extern "Rust" {
    fn add_acmd_load_hook(callback: fn(&mut L2CFighterCommon));
}
