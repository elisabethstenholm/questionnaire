# Questionnaire

A questionnaire is inherently dependently typed. This repository contains a
prototype implementation of questionnaires in the dependently typed functional
programming language Idris2.

This repository uses my fork of the package
[idris2-dom-mvc](https://github.com/elisabethstenholm/idris2-dom-mvc). **Note:
The current setup expects the local clone of my `idris2-dom-mvc` fork to live in
the same folder as this repository. You can change this by changing the path
variable in the `pack.toml` file.**

To compile, you need to install
[pack](https://github.com/stefan-hoeck/idris2-pack) and then run `pack build
questionnaire.ipkg` from the root of the repository. The questionnaire will be
available at the file `index.html`.
