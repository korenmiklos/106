# wget --no-host-directories --directory-prefix=html --cut-dirs=4 -x -i szavazokorok_results.txt 
cat szavazokorok_results.txt | parallel wget --no-host-directories --directory-prefix=html --cut-dirs=4 -x  
#cat url.txt | parallel wget --no-host-directories --directory-prefix=html --cut-dirs=4 -x  
