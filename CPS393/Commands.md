#cd 
```bash
cd [filename] (change directory)
```
#chmod 
```bash
# Changes the mode/permission bits of each given file according to a mode
# Mode is either a symbolic representation or octal number
# user and group have read write execute permissions on "file"
man chmod
chmod "filename" ug=rwx 
chmod 070 "filenanme" # User and owner have no permission on file while group has read write execute access
```
#ls
```bash
ls -l # List all files with permissions
ls -al # Lists all files with their permissions
ls -a # List all files including hidden
ls -t # Lists all files in order sorted from latest modified

```
#pwd
```bash
pwd # Returns the path of the current directory
```
#cp
```bash
cp [file] [destination] # Copies the file to another directory
```
#mkdir
```bash
mkdir [filepath/filename] # Make a new folder in a designated path
```
#mv
```bash
mv [filename] [filepath] # Moves the file to another path
```
#cat
```bash
cat [filename/filepath] # Display contents of file without paging
```
#echo
```bash
echo "anything" # prints the given string
```
#tac 
```bash
tac [filepath/filename] # cat file in reverse order last -> first 
```
#less
```bash
less [filename] # Similar to more but has more features
```
#printf
```bash
printf "This is my age %d\n" 1 # Print like C with formatted string
```
#head
```bash
head [filename] # Prints the first 10 lines of given file
head -lineNumber [filename] # Line offset from the top
```
#tail
```bash
tail [filename] # Prints the last 10 lines of given file
tail -lineNumber [filename] # Line number from the bottom
```
#rev
```bash
rev [filename] # Prints each line in reverse
```
#sort
```bash
sort [filename] # Sorts input lines
```
#uniq
```bash
uniq [filename] # Print each line but avoid duplicates
```
#diff
```bash
diff [filename or ""] [filename or ""] # shows difference between 2 files (Shows nothing if identical)
```
#touch 
```bash
touch [filename] # create files
```
#wc
```bash
wc 'any word' or [filename] # Gives size / counting words
```
#rmdir
```bash
rmdir [foldername] # remove a directory
```
#rm 
```bash
rm [filename] # Removes a file
```
#more
```bash
more [filename] # Cat with pages (spacebar for next pg, enter for next line and q for quitting)
```
#tee
```bash
tee text.txt # Save in a file, used with piping usually
```

#grep
# GREP
```sh
# GREP GLOBAL REGULAR EXPRESSION PRINT
# General format
grep [option] [expression] [filename(s)]
# -e is a must when you want to use regex and -f is a must for searching files

############## METACHARACTERS #################
. # any character, except new line
* # 0 or more repititions of previous character
^ # beginning of line
$ # end of line
[ ... ] # any character inside the brackets (like glob)
[^ ...] # Any character not inside the brackets
\{m\} # == m repitions of the previous character
\{m,\} # >= m repititions of the previous character
\{m,n\} # m <= repitions of characters <= n
\< # Beginning of a word
\> # end of word
\(str\) # group string str
# NOTE: \ can also be used as an escape character
| # Or
+ # One or more
```

#pipe
# Piping Commands
```bash
ls -t | head # Prints the 10 latest files modified
ls -t | head >"Output Filename" # Puts the output in a file
last | head # Show the last 10 logins on this machine
cat "filename" | head -lineNumber | tail -lines # Print the last tail number of lines starting from where head ends
cat "filename" | head -lineNumber | tee theselines | tail -lines # Also save the first lines on the file called theselines
```

# Practice Commands
```bash
* # String of characters
? # Single character
[...] # Single char from set of chars within the brackets
# These are examples char sequences with glob constructs
[:alnum:] [:alpha:] [:blank:] [:cntrl:]
[:digit:] [:graph:] [:lower:] [:print:]
[:punct:] [:space:] [:upper:] [:xdigit:]
ls [[:upper:][:upper:]]* # Would search for an upper case character followed by all possible combinations
ls [[:upper:]][[:upper:]]* # Would search for an upper case character followed by all possible combinations
~+ # Current working dir
mkdir newfile; cd newfile; touch newFile.txt # Use multiple commands at once
find [path] [option] filename
```
# REGEX in the shell
```sh
+(exp) # >=1
?(exp) # <=1
*(exp) # >=0
@(exp1|exp2|...) # Works like an or condition
!(exp) # Anything that does not match exp
```