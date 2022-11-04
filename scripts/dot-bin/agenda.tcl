#!/usr/bin/env tclsh8.6

array set colors {
    black   "\033\[1;30m"
    red     "\033\[1;31m"
    green   "\033\[1;32m"
    yellow  "\033\[1;33m"
    blue    "\033\[1;34m"
    magenta "\033\[1;35m"
    cyan    "\033\[1;36m"
    white   "\033\[1;37m"
    end     "\033\[0m"
}

proc print {text in color} {
    global colors
    puts -nonewline $colors($color)
    puts -nonewline $text
    puts $colors(end)
}

if {[llength $argv] > 0 & [lindex $argv 0] eq {next}} {
    set next next
    set time [clock scan "next week"]
} else {
    set next {}
    set time [clock seconds]
}

lassign [clock format $time -format "%W %d %m %Y"] \
    week day month year 

if {$next ne {}} {
    set day -1
}

set calendar [open "/home/anton/data/planing/calendar.txt"]
set lines [split [read $calendar] "\n"]
close $calendar

foreach line $lines {
    # year
    if {[string first "$year  " $line] == 0} {
        print $line in red
        continue
    }
    # month
    if {[string first "$year-$month  " $line] == 0} {
        print $line in yellow
        continue
    }
    # today, print green
    if {[string first "$year-$month-$day " $line] == 0} {
        print $line in green
        continue
    }
    # another days of week
    if {[string first "w$week" $line] > -1 & \
        [string first "$year-" $line] == 0} {
        puts $line
        continue
    }
}
