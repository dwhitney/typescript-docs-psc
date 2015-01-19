all:
		find bower_components/**/src/** src -name '*.purs' | xargs psc --module Main --main=Main > /tmp/tsc-docs.js 
		echo '#!/usr/bin/env node' | cat - /tmp/tsc-docs.js > index.js
