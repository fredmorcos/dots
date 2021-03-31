local wezterm = require 'wezterm';

local launch_menu = {
  {
    label = "ConfMerge",
    args = {"./confmerge"},
    cwd = "/home/fred/Workspace/dots",
  },
}

return {
  term = "wezterm",
  color_scheme = "Violet Light",
  initial_rows = 40,
  initial_cols = 140,
  font = wezterm.font("JuliaMono"),
  font_size = 11.5,
  line_height = 0.9,
  bold_brightens_ansi_colors = true,
  -- font_antialias = "Subpixel",
  -- font_hinting = "VerticalSubpixel",
  harfbuzz_features = {"liga=0", "dlig=0", "calt=0", "clig=0"},
  hide_tab_bar_if_only_one_tab = true,
  enable_scroll_bar = true,
  scrollback_lines = 10000,
  window_padding = {
    left = 2,
    right = 6,
    top = 2,
    bottom = 2,
  },
  launch_menu = launch_menu,
}
