local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.enable_scroll_bar = true
config.scrollback_lines = 50000


return config