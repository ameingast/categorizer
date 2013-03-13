# About

Categorizer is a simple Web API with a basic UI to match text documents
against SKOS dictionaries and provide a simple visualization of
the extracted categories/concepts.

## Running Categorizer Locally 
_make_ compiles the application and installs all dependencies
via cabal.

_make run_ executes the compiled binary on the port 8080; a different
port can be configured through the system environment variable _PORT_.

## Running Categorizer on Heroku
In order to deploy on Heroku, you need a Heroku account and a configured
shell environment. 

After checking out and building Categorizer locally (to make sure
everything works), execute the following command:

    _heroku create --stack=cedar --buildpack https://github.com/ameingast/heroku-buildpack-haskell.git_

This command initializes a Heroku application and adds a Haskell buildpack to
the project.

In order to deploy the application, execute the following commands: 
    _git push heroku master_

After deployment you can start the application with: _heroku ps:scale web=1_

## Caveats
Categorizer is still in active development and currently does not have any
support for persisting data on Heroku (it will be added later).

## Resources
* [SKOS](http://www.w3.org/2004/02/skos/)
* [WAI](http://hackage.haskell.org/packages/archive/wai/latest/doc/html/Network-Wai.html)
* [Heroku](http://www.heroku.com)
