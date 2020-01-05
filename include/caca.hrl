%% ----------------------------------
%% Erlang bindings for libcaca
%% ----------------------------------

-ifndef(caca_hrl).
-define(caca_hrl, true).

%% caca_color
%% __________

-define(BLACK, 16#00).
-define(BLUE, 16#01).
-define(GREEN, 16#02).
-define(CYAN, 16#03).
-define(RED, 16#04).
-define(MAGENTA, 16#05).
-define(BROWN, 16#06).
-define(LIGHTGRAY, 16#07).
-define(DARKGRAY, 16#08).
-define(LIGHTBLUE, 16#09).
-define(LIGHTGREEN, 16#0A).
-define(LIGHTCYAN, 16#0B).
-define(LIGHTRED, 16#0C).
-define(LIGHTMAGENTA, 16#0D).
-define(YELLOW, 16#0E).
-define(WHITE, 16#0F).
-define(DEFAULT, 16#10).
-define(TRANSPARENT, 16#20).

%% caca_style
%% __________

-define(BOLD, 16#01).
-define(ITALICS, 16#02).
-define(UNDERLINE, 16#04).
-define(BLINK, 16#08).

%% caca_event_type
%% _______________

-define(EVENT_NONE, 16#0000).
-define(EVENT_KEY_PRESS, 16#0001).
-define(EVENT_KEY_RELEASE, 16#0002).
-define(EVENT_MOUSE_PRESS, 16#0004).
-define(EVENT_MOUSE_RELEASE, 16#0008).
-define(EVENT_MOUSE_MOTION, 16#0010).
-define(EVENT_RESIZE, 16#0020).
-define(EVENT_QUIT, 16#0040).
-define(EVENT_ANY, 16#ffff).

%% caca_key
%% ________

-define(KEY_UNKNOWN, 16#00).
-define(KEY_CTRL_A, 16#01).
-define(KEY_CTRL_B, 16#02).
-define(KEY_CTRL_C, 16#03).
-define(KEY_CTRL_D, 16#04).
-define(KEY_CTRL_E, 16#05).
-define(KEY_CTRL_F, 16#06).
-define(KEY_CTRL_G, 16#07).
-define(KEY_BACKSPACE, 16#08).
-define(KEY_TAB, 16#09).
-define(KEY_CTRL_J, 16#0A).
-define(KEY_CTRL_K, 16#0B).
-define(KEY_CTRL_L, 16#0C).
-define(KEY_RETURN, 16#0D).
-define(KEY_CTRL_N, 16#0E).
-define(KEY_CTRL_O, 16#0F).
-define(KEY_CTRL_P, 16#10).
-define(KEY_CTRL_Q, 16#11).
-define(KEY_CTRL_R, 16#12).
-define(KEY_PAUSE, 16#13).
-define(KEY_CTRL_T, 16#14).
-define(KEY_CTRL_U, 16#15).
-define(KEY_CTRL_V, 16#16).
-define(KEY_CTRL_W, 16#17).
-define(KEY_CTRL_X, 16#18).
-define(KEY_CTRL_Y, 16#19).
-define(KEY_CTRL_Z, 16#1A).
-define(KEY_ESCAPE, 16#1B).
-define(KEY_DELETE, 16#7F).
-define(KEY_UP, 16#111).
-define(KEY_DOWN, 16#112).
-define(KEY_LEFT, 16#113).
-define(KEY_RIGHT, 16#114).
-define(KEY_INSERT, 16#115).
-define(KEY_HOME, 16#116).
-define(KEY_END, 16#117).
-define(KEY_PAGEUP, 16#118).
-define(KEY_PAGEDOWN, 16#119).
-define(KEY_F1, 16#11A).
-define(KEY_F2, 16#11B).
-define(KEY_F3, 16#11C).
-define(KEY_F4, 16#11D).
-define(KEY_F5, 16#11E).
-define(KEY_F6, 16#11F).
-define(KEY_F7, 16#120).
-define(KEY_F8, 16#121).
-define(KEY_F9, 16#122).
-define(KEY_F10, 16#123).
-define(KEY_F11, 16#124).
-define(KEY_F12, 16#125).
-define(KEY_F13, 16#126).
-define(KEY_F14, 16#127).
-define(KEY_F15, 16#128).

-endif.
