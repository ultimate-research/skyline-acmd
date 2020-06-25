# Skyline ACMD

A library for working with ACMD using skyline-rs and skyline-smash.

### Features

* Run ACMD code inline with Rust code
* Automatic conversion from Rust types to lua types
* Emulate `is_execute`, `frame`, and `wait` using the same syntax
* Optional syntax additions (semicolons, function argument labels)
* Inline Rust code blocks
* Recursive ACMD blocks inside of inline Rust code blocks
* ACMD if statements
* Call Module functions
* Call ACMD functions

To add to project, just add the following to your dependencies in `Cargo.toml`:

```toml
acmd = { git = "https://github.com/ultimate-research/skyline-acmd.git" }
```

### Example

```rust
use acmd::acmd;

acmd!(lua_state, {
    frame(16)
    AttackModule::clear_all() // clear all previous hitboxes
    if(is_execute) {
        ATTACK(
            ID=0, Part=0, Bone=hash40("arml"), 19.0, 361, 80, 0, 30, 113.0, 3.2, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0,
            ATTACK_SETOFF_KIND_ON, ATTACK_LR_CHECK_F, ATTACK_LR_CHECK_F, ATTACK_LR_CHECK_F,
            ATTACK_LR_CHECK_F, ATTACK_LR_CHECK_F, false, false, false, false, true,
            COLLISION_SITUATION_MASK_GA, COLLISION_CATEGORY_MASK_ALL, COLLISION_PART_MASK_ALL,
            false, hash40("collision_attr_fire"), ATTACK_SOUND_LEVEL_M, COLLISION_SOUND_ATTR_PUNCH,
            ATTACK_REGION_PUNCH
        )
    }
    rust {
        for i in 1..4 {
            acmd!({
                ATTACK(
                    ID=i, Part=0, Bone=hash40("arml"), 19.0, 361, 80, 0, 30, 113.0, 3.2, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0,
                    ATTACK_SETOFF_KIND_ON, ATTACK_LR_CHECK_F, ATTACK_LR_CHECK_F, ATTACK_LR_CHECK_F,
                    ATTACK_LR_CHECK_F, ATTACK_LR_CHECK_F, false, false, false, false, true,
                    COLLISION_SITUATION_MASK_GA, COLLISION_CATEGORY_MASK_ALL, COLLISION_PART_MASK_ALL,
                    false, hash40("collision_attr_fire"), ATTACK_SOUND_LEVEL_M, COLLISION_SOUND_ATTR_PUNCH,
                    ATTACK_REGION_PUNCH
                )
            });
        }
    }
    sv_kinetic_energy::add_speed(1.0)

});
```
To get a better idea of the context in which to use this macro, see [the full example](https://gist.github.com/jugeeya/27b902865408c916b1fcacc486157f79).
