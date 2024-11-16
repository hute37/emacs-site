-- Pull in the wezterm API
local wezterm = require 'wezterm'


local config = wezterm.config_builder()

-- This is where you actually apply your config choices
--


config.wsl_domains = {
  {
    -- The name of this specific domain.  Must be unique amonst all types
    -- of domain in the configuration file.
    name = 'WSL:Ubuntu',

    -- The name of the distribution.  This identifies the WSL distribution.
    -- It must match a valid distribution from your `wsl -l -v` output in
    -- order for the domain to be useful.
    distribution = 'Ubuntu',
  },
}
--config.default_domain = 'WSL:Ubuntu'



-- You can specify some parameters to influence the font selection;
-- for example, this selects a Bold, Italic font variant.
-- config.font = wezterm.font('JetBrains Mono', { weight = 'Bold', italic = true })
--


config.font = wezterm.font_with_fallback {
  'Hack Nerd Font Mono',
  'Fira Code',
  'DengXian',
  'Consolas',
}

config.font_size = 18.0


config.hide_tab_bar_if_only_one_tab = true

-- config.tab_bar_at_bottom = true


-- For example, changing the color scheme:
config.color_scheme = 'Argonaut'

config.window_background_gradient = {
  colors = { '#002540', '#000000' },
  -- colors = { '#EEBD89', '#D13ABD' },
  -- Specifies a Linear gradient starting in the top left corner.
  orientation = { Linear = { angle = -45.0 } },
}

config.window_padding = {
  left = 20,
  right = 20,
  top = 15,
  bottom = 5,
}


config.initial_cols = 110
config.initial_rows = 30


-- and finally, return the configuration to wezterm
return config

