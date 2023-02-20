# UTAM Java Repository

![Maven Central](https://img.shields.io/maven-central/v/com.salesforce.utam/utam-java?style=for-square)
![GitHub](https://img.shields.io/github/license/salesforce/utam-java)
![Snyk Vulnerabilities for GitHub Repo](https://img.shields.io/snyk/vulnerabilities/github/salesforce/utam-java?style=for-square)
[![Snyk](https://snyk-widget.herokuapp.com/badge/mvn/com.salesforce.utam/utam-java/badge.svg)](https://snyk.io/vuln/maven:com.salesforce.utam:utam-java?utm_medium=referral&utm_source=badge&utm_campaign=snyk-widget)

Welcome to the **UTAM Java Repository**!

The [UI Test Automation Model (UTAM) project](https://utam.dev) allows a
developer to create and use Page Objects for automating web pages through
the browser. What makes UTAM unique is that it relies on a declarative
description format to define the Page Objects. This format is independent
of any specific programming language, and defines the Page Objects by use of a
JSON grammar. This approach allows the same Page Object definitions to be used
to generate Page Objects for use in any programming language for which there is
a UTAM compiler and runtime library. This repository contains the compiler and
runtime library for the Java programming language.

Please note that this repository does not contain actual UTAM Page Object
declarative description files. It only contains the UTAM compiler and runtime
library for Java.

## Getting Started
The UTAM project is developed and built using the Apache Maven project. To
get started, you will need the following prerequisites installed:

* A Java Development Kit (JDK) for Java 11. Salesforce recommends
  [OpenJDK](https://openjdk.java.net).
* The [Maven build system](https://maven.apache.org/install.html).

Once you have forked and cloned this repository, in a terminal window, you
open a terminal window, navigate into the directory where you have cloned
the repository, and execute

    mvn clean package

This will update or install the required dependencies, and build the project.

Note that when building the project, either from a command line or within
an IDE, compilation is expected to be done on the entire project, not on
individual modules. In the command line, this means the above command should
be executed at the root directory of the project clone; in an IDE, you should
execute compilation on the entire project in the IDE.

## Contributing
Contributions are handled using GitHub pull requests. All new code must be
accompanied by unit tests, and all existing unit tests must pass before
acceptance. Contributors must sign the [Salesforce Contributor License
Agreement](https://cla.salesforce.com/sign-cla) before their contribution
can be accepted.

## License
The UTAM Java compiler is licensed under the [MIT license](LICENSE).
