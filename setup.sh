#!/usr/bin/env sh

if test ! -e deps.edn
	 then
		 echo "You must run setup.sh from the project root"
		 exit 1
fi

test -d classes || mkdir classes

clojure -e "(compile 'falloleen.hosts.jfx.classes)"
