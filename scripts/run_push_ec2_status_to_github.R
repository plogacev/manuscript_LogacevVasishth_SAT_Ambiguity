#!/usr/bin/Rscript
update_interval_sec <- 60*60

while (TRUE) {
  system("git commit -a -m 'Automatic update'")
  system("git push")

  Sys.sleep(update_interval_sec)
}
