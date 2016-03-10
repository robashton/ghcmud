all: js

js: app.js src/*.js
	browserify app.js --debug --transform reactify > ./app-built.js
