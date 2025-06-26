# Get editor completions based on the config schema
"$schema" = 'https://starship.rs/config-schema.json'

add_newline = false
command_timeout = 100
scan_timeout = 10
continuation_prompt = '▶▶ '
format = '$battery$jobs$directory$git_branch$git_status$character'

[c]
disabled = true

[cpp]
disabled = true

[python]
disabled = true

[character]
error_symbol = '[✖](bold red)'

[directory]
truncation_length = 1
truncate_to_repo = false
fish_style_pwd_dir_length = 1

[env_var.CC]
variable = 'CC'

[env_var.CFLAGS]
variable = 'CFLAGS'

[env_var.CXX]
variable = 'CXX'

[env_var.CXXFLAGS]
variable = 'CXXFLAGS'
