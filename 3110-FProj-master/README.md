# 3110-FProj
Final Project for CS 3110: Othello

In order to compile and run this project you will need to do a few things. 
This project requires lablgtk which requires gtk+. 

Installation steps:

1) If you do not have gtk+ then do one of the following:
  If you are using OSX then run the command "brew install gtk+" 
  If you are using Ubuntu then run the commands 
  "sudo apt-get install libgtk2.0-0" and "sudo apt-get install libgtk2.0-dev"
  If neither of these work visit https://www.gtk.org/download/index.php and follow
  the instructions to download gtk+ 2 for your operating system.
2) Once gtk+ has been properly installed install lablgtk by running the command
  "opam install lablgtk"
3) If you would like to run our test cases and do not have Ounit installed
  run the command "opam install ounit"

Run steps:
1) to build and launch our game run the command "make game"
2) to run our test suit run the command "make test"
