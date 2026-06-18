local wezterm = require 'wezterm'
local config = wezterm.config_builder()

-- config.color_scheme = 'Duotone Dark'

config.enable_scroll_bar = true
config.scrollback_lines = 50000

return config