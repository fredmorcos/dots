# dots
dotfiles (configuration files and scripts)

# Notes

* LUKS Performance Improvements (Enabled discards and disable WorkQueues)
- ~cryptsetup refresh --allow-discards --perf-no_read_workqueue --perf-no_write_workqueue --persistent root~
- ~cryptsetup refresh --allow-discards --perf-no_read_workqueue --perf-no_write_workqueue --persistent home~
