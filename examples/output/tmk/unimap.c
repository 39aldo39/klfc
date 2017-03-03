#include "unimap_trans.h"
#include "action_util.h"
#include "action_layer.h"

enum function_id {
    F_MODIFIER,
};

enum modifier_id {
    MOD_SHIFT,
    MOD_SHIFT_L,
    MOD_SHIFT_R,
    MOD_EXTEND,
};

#define AC_FN0 ACTION_FUNCTION_OPT(F_MODIFIER, MOD_EXTEND)
#define AC_FN1 ACTION_FUNCTION_OPT(F_MODIFIER, MOD_SHIFT_L)
#define AC_FN2 ACTION_FUNCTION_OPT(F_MODIFIER, MOD_SHIFT_R)
#define AC_FN3 ACTION_FUNCTION_OPT(F_MODIFIER, MOD_SHIFT)
#define AC_FN4 ACTION_MODS_KEY(MOD_BIT(KC_LCTL), KC_Z)
#define AC_FN5 ACTION_MODS_KEY(MOD_BIT(KC_LCTL), KC_X)
#define AC_FN6 ACTION_MODS_KEY(MOD_BIT(KC_LCTL), KC_C)
#define AC_FN7 ACTION_MODS_KEY(MOD_BIT(KC_LCTL), KC_V)

#ifdef KEYMAP_SECTION_ENABLE
const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] __attribute__ ((section (".keymap.keymaps"))) = {
#else
const action_t actionmaps[][UNIMAP_ROWS][UNIMAP_COLS] PROGMEM = {
#endif
    // None
    [0] = UNIMAP(
               F13, F14, F15, F16, F17, F18, F19, F20, F21, F22, F23, F24,
     ESC,       F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9, F10, F11, F12,          PSCR,SLCK,PAUS,         VOLD,VOLU,MUTE,
     GRV,   1,   2,   3,   4,   5,   6,   7,   8,   9,   0,MINS, EQL,JYEN,BSPC,      INS,HOME,PGUP,    NLCK,PSLS,PAST,PMNS,
     TAB,   Q,   W,   F,   P,   G,   J,   L,   U,   Y,SCLN,LBRC,RBRC,     BSLS,      DEL, END,PGDN,      P7,  P8,  P9,PPLS,
     FN0,   A,   R,   S,   T,   D,   H,   N,   E,   I,   O,QUOT,     NUHS, ENT,                          P4,  P5,  P6,PCMM,
     FN1,MINS,   Z,   X,   C,   V,   B,   K,   M,COMM, DOT,SLSH,       RO, FN2,            UP,           P1,  P2,  P3,PENT,
    LCTL,LGUI,LALT,MHEN,           SPC,          HENK,KANA,RALT,RGUI, APP,RCTL,     LEFT,DOWN,RGHT,      P0,     PDOT,PEQL
    ),
    // Extend
    [1] = UNIMAP(
              TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,
    CAPS,     MPLY,MPRV,MNXT,MSTP,MUTE,VOLD,VOLU,MSEL,WHOM,FIND,  NO,CALC,          TRNS,TRNS,TRNS,         TRNS,TRNS,TRNS,
      NO,  F1,  F2,  F3,  F4,  F5,  F6,  F7,  F8,  F9, F10, F11, F12,TRNS,TRNS,     TRNS,TRNS,TRNS,    TRNS,TRNS,TRNS,TRNS,
    TRNS, ESC,WH_U,WBAK,WFWD,MS_U,PGUP,HOME,  UP, END, DEL, ESC, INS,     WFAV,     TRNS,TRNS,TRNS,    TRNS,TRNS,TRNS,TRNS,
    TRNS,LALT,WH_D, FN3,LCTL,MS_D,PGDN,LEFT,DOWN,RGHT,BSPC, APP,     NUHS,TRNS,                        TRNS,TRNS,TRNS,TRNS,
    TRNS,  NO, FN4, FN5, FN6, FN7,BTN1,BTN2,BTN3,MS_L,MS_R,  NO,     TRNS,TRNS,          TRNS,         TRNS,TRNS,TRNS,TRNS,
    TRNS,TRNS,TRNS,TRNS,           ENT,          TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     TRNS,TRNS,TRNS,    TRNS,     TRNS,TRNS
    ),
    // Shift+Extend
    [2] = UNIMAP(
              TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,
    TRNS,       NO,MRWD,MFFD,EJCT,WREF,  NO,  NO,SLEP,  NO,MAIL,TRNS,  NO,          TRNS,TRNS,TRNS,         TRNS,TRNS,TRNS,
    TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     TRNS,TRNS,TRNS,    TRNS,TRNS,TRNS,TRNS,
    TRNS,TRNS,WH_R,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     MYCM,     TRNS,TRNS,TRNS,    TRNS,TRNS,TRNS,TRNS,
    TRNS,TRNS,WH_L,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     NUHS,TRNS,                        TRNS,TRNS,TRNS,TRNS,
    TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     TRNS,TRNS,          TRNS,         TRNS,TRNS,TRNS,TRNS,
    TRNS,TRNS,TRNS,TRNS,          TRNS,          TRNS,TRNS,TRNS,TRNS,TRNS,TRNS,     TRNS,TRNS,TRNS,    TRNS,     TRNS,TRNS
    ),
};

#define MOD_SHIFT_MASK (MOD_BIT(KC_LSFT)|MOD_BIT(KC_RSFT))

enum virtual_mod_mask {
    VIRTUAL_MOD_EXTEND_MASK = 1,
};

uint8_t virtual_mods = 0;

const uint8_t layer_states[] = {
    0x1, // None
    0x1, // Shift
    0x3, // Extend
    0x7, // Shift+Extend
};

void action_function(keyrecord_t *record, uint8_t id, uint8_t opt) {
    uint8_t pressed = record->event.pressed;
    switch (id) {
        case F_MODIFIER:
            // Set the new modifier
            switch (opt) {
                case MOD_SHIFT: pressed ? add_key(KC_LSFT) : del_key(KC_LSFT); break;
                case MOD_SHIFT_L: pressed ? add_key(KC_LSFT) : del_key(KC_LSFT); break;
                case MOD_SHIFT_R: pressed ? add_key(KC_RSFT) : del_key(KC_RSFT); break;
                case MOD_EXTEND: pressed ? (virtual_mods |= VIRTUAL_MOD_EXTEND_MASK) : (virtual_mods &= ~VIRTUAL_MOD_EXTEND_MASK); break;
            }

            // Update the layer
            uint8_t mods = get_mods();
            uint8_t layer_index = 0;
            layer_index |= mods & MOD_SHIFT_MASK ? 1 : 0;
            layer_index |= virtual_mods << 1;
            layer_clear();
            layer_or(layer_states[layer_index]);
            break;
    }
}
