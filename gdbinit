set history save
set history filename ~/.gdbhist
set print array on
set print pretty on
set pagination off
set confirm off
set breakpoint pending on
# set disable-randomization

# INTERACTIVE SETTINGS AND COMMANDS

# C-x a     to enable TUI mode
# C-x 2     to switch in TUI mode
# C-p/C-n   go to previous and next history elements

# Use this to debug a forked/exec'ed process.

# set detach-on-fork off
# set follow-fork-mode child/parent
# set follow-exec-mode new/same

# Inferiors:

# info inferiors
# inferior N
# add-inferior <name>
# clone-inferior

# This can help with debugging  multi-threaded programs. When a thread
# is  stopped  by a  breakpoint,  this  will  allow other  threads  to
# continue. Use "break <func-name>" to break a specific thread and use
# "continue -a" to continue all threads.

# set non-stop on
# continue -a

# Watchpoints:

# watch foo               # stop when foo is modified
# watch -l foo            # watch location
# rwatch foo              # stop when foo is read
# watch foo thread 3      # stop when thread 3 modifies foo
# watch foo if foo > 10   # stop when foo meets condition

# Threads:

# thread apply 1-4 print $sp
# thread apply all backtrace
# thread apply all backtrace full

# Printf debugging:

# dprintf <function>,<format-string>,args
# set dprintf-style gdb/call/agent
# set dprintf-function fprintf
# set dprintf-channel mylog           # what gets passed to function

# Catchpoints:

# catch catch             # catch C++ exceptions
# catch syscall foo       # stop at syscall foo
# catch syscall 100       # stop at syscall number 100

# Other:

# call foo()          # call foo() inside the inferior process
# tbreak              # temporary breakpoint
# rbreak              # regex breakpoint (ie, mylist_*)
# command             # commands to be executed when breakpoint is hit
# silent              # suppress output when running commands
# save breakpoints    # save breakpoits to a gdb script
# save history
# info line foo.c:42  # show $pc for line
# info line * $pc     # show line begin/end for current $pc
