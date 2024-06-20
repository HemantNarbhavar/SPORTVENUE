# Installation Step

$ git clone https://gitlab.com/HemantNarbhavar/sportvenue.git

$ cd sportvenue

make change in run_cabal.sh file
gnome-terminal -- bash -c "psql -d postgres -a -f database/schema.sql; exec bash"
(role is created in postgresql then it will work with 'psql' command)


$ chmod +x run_cabal.sh

$ ./run_cabal.sh