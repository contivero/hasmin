#!/bin/bash

# Usage: ./upload-docs <package-name> <version>

dist=`stack path --dist-dir --stack-yaml ./stack.yaml 2> /dev/null`

echo -e "Generating documentation..."
stack haddock 2> /dev/null

if [ "$?" -ne "0" ]; then
  echo -e "Problem when calling 'stack haddock'"
  exit 1
fi


docdir=$dist/doc/html
cd $docdir
doc=$1-$2-docs
echo -e "Compressing documentation from $docdir for $1-$2"
cp -r $1 $doc
tar --format=ustar -cvf $doc.tar $doc ; gzip -9 $doc.tar
echo -e "Uploading to Hackage..."
read -p "Hackage username: " username
read -p "Hackage password: " -s password
echo ""
curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@$doc.tar.gz" "https://$username:$password@hackage.haskell.org/package/$1-$2/docs"
exit $?
