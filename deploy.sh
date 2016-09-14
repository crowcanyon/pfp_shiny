#!/usr/bin/expect

set user [lindex $argv 0]; # Grab the first command line parameter
set pass [lindex $argv 1]; # Grab the first command line parameter

spawn rsync -avzr --size-only --delete ./ $user@macaw.crowcanyon.org:/srv/shiny-server/pfp/ 
expect "password:"
send "$pass\n"
expect eof
if [catch wait] {
    puts "rsync failed"
    exit 1
}
exit 0