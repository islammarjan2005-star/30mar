## Quality assurance checklist 

Adapted from the quality assurance checklist from [the quality assurance of code for analysis and research guidance](https://best-practice-and-impact.github.io/qa-of-code-guidance/intro.html). This is designed to evaluate a project's codebase upon the completion of the project. Proportional quality assurance should be carried out on all code which forms part of a project’s codebase. The full checklist and may not be appropriate for all projects. 

**QA level for project:** [select minimum/good practice/best practice] _All projects should meet the minimum criteria. If assessing against 'Good' or 'Best practice' criteria, ~~strikethrough~~ any elements which are not relevant for your project  (by wrapping text with `~~`)._

**ACTION FOR DEVELOPER:** This checklist is for you to conduct a self-assessment at review points, before handover to the assurer. This will help them understand the overall quality of the code and areas where you already know you may need to develop further.

**ACTION FOR ASSURER:** In QA documentation note where you believe more could be done to meet the elements of the checklist.
 


## A.1. Project Documentation 

 

_Project documentation should be comprehensive and allow the project to be viewed in isolation, with links to connected projects where relevant_ 

<details><summary>Click to expand</summary>

### Minimum 

A.1.1 Documentation has a clear structure:

- [ ] A README file details the purpose of the project and basic installation instructions. 

- [ ] Wiki and documentation pages are clearly set out and contain useful information for developers.

- [ ] Project governance including the roles and responsibilities of team members are clearly defined. 

A.1.2 Quality assurance:

- [ ] The extent of analytical quality assurance conducted on the project is clearly documented (e.g. in a QA log or MRs). 

A.1.3 Issues tracker, change log and acceptance criteria:

- [ ] An issue tracker (e.g GitLab boards) is used to record development tasks. 


### Good Practice 

<details><summary>Click to expand</summary><br />

A.1.1 Documentation has a clear structure:

- [ ] Example of how to use functionality and bespoke packages are documented for developers and users. 

- [ ] Where appropriate, guidance for prospective contributors is available including a code of conduct. 

A.1.3 Issues tracker, change log and acceptance criteria:

- [ ] New issues or tasks are guided by users’ needs and stories. 

- [ ] Issues templates are used to ensure proper logging of the title, description, labels and comments. 

- [ ] Acceptance criteria (a set of statements with a clear pass/fail results, you should not merge until these are satisfied) are noted for issues and tasks. Fulfilment of acceptance criteria is recorded near to the code. 

A.1.4 Clear versions:

- [ ] Releases of the project (if applicable) used for reports, publications, or other outputs are versioned using a standard pattern such as MAJOR.MINOR.PATCH [semantic versioning](https://semver.org/). 

- [ ] A summary of changes to functionality are documented in a changelog following releases (this could be through the use of GitLab tags). The changelog is available to users. 
</details>
 

### Best Practice 
<details><summary>Click to expand</summary><br />

A.1.1 Documentation has a clear structure:

- [ ] Copyright and licenses are specified for both documentation and code. 

- [ ] Instructions for how to cite the project are given. 

- [ ] Design certificates confirm that the design is compliant with requirements. 

- [ ] If appropriate, the software is fully specified. 
</details>
 
</details>
 

## A.2. Code Documentation 

 

_Code documentation is clear and allows the user to run and reuse the code without relying on the developer_ 

<details><summary>Click to expand</summary>


### Minimum 

A.2.1 Fully commented code:

- [ ] Comments are used to describe why code is written in a particular way, rather than describing what the code is doing. 

- [ ] Code is not commented out to adjust which lines of code run. 

- [ ] Assumptions of the analysis are noted in the comments and linked to the relevant assumptions log or documentation.

A.2.2 Docstrings:

- [ ] Python code is [documented using docstrings](https://www.python.org/dev/peps/pep-0257/). R code is [documented using `roxygen2` comments](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html). 

- [ ] All functions and classes are documented to describe what they do, what inputs they take and what they return. 

A.2.3 Access to version control:

- [ ] Code is version controlled using Git. 

- [ ] Code is committed regularly, preferably when a discrete unit of work has been completed. 

- [ ] An appropriate branching strategy is defined and used throughout development. 

A.2.4 Requirements, environments or containers:

- [ ] Required passwords, secrets and tokens are documented, but are stored outside of version control. 

- [ ] Required libraries and packages are documented, including their versions. 

 
### Good Practice 

<details><summary>Click to expand</summary><br />

A.2.2 Docstrings:

- [ ] Human-readable (preferably HTML) documentation is generated automatically from code documentation. 

A.2.3 Access to version control:

- [ ] Documentation is hosted on GitLab for easy access. 

- [ ] Committing standards are followed such as appropriate commit summary and message supplied. 

- [ ] Commits are tagged at significant stages. This is used to indicate the state of code for specific releases or model versions. 

A.2.4 Requirements, environments or containers:

- [ ] Package dependencies are managed using an environment manager such as [virtualenv for Python](https://virtualenv.pypa.io/en/latest/) or [renv for R](https://rstudio.github.io/renv/articles/renv.html).  

- [ ] There are as few dependencies as possible to minimise environment issues if packages/software are updated. 

- [ ] Where appropriate, code runs independent of operating system (e.g. suitable management of file paths). 

</details>

### Best Practice 

<details><summary>Click to expand</summary><br />

A.2.3 Access to version control:

- [ ] Continuous integration is applied through tools such as GitLab CICD tools, to ensure that each change is integrated into the workflow smoothly. 

A.2.4 Requirements, environments or containers:

- [ ] Dependencies are managed separately for users, developers, and testers. 

- [ ] Working operating system environments are documented. 

- [ ] [Configuration](https://best-practice-and-impact.github.io/qa-of-code-guidance/configuration.html) (a description of how your code runs when you execute it, including inputs and filepaths) is written as code, and is clearly separated from code used for analysis. 

- [ ] The configuration used to generate particular outputs, releases and publications is recorded. 

- [ ] Example configuration files are provided. 

- [ ] Docker containers or virtual machine builds are available for the code execution environment and these are version controlled. 

</details>

</details>

## B.1. Project Structure and Clarity 

_Project directories use clear and standardised structures_ 
 
<details><summary>Click to expand</summary>


### Minimum 

B.1.1 Standard directory:

- [ ] A clear, standard directory structure is used to separate input data, outputs, code and documentation. 

- [ ] Packages follow a standard directory structure (e.g. Golem for R). 

B.1.2 Use of different platforms:

- [ ] Where documentation, code and outputs are saved on different platforms there is clear guidance on where everything is saved. 

</details>

## B.2. Code Structure and Clarity 

 
_Code structure is easy to follow and ensures the project can be replicated by another analyst_ 

<details><summary>Click to expand</summary>


### Minimum 

B.2.1 Consistent convention:

- [ ] Names used in the code are informative and concise. 

- [ ] Repetition in the code is minimalised. Individual pieces of logic are written as functions. Classes are used if more appropriate. 

- [ ] Low level functions and classes carry out one specific task. As such, there is only one reason to change each function. 

- [ ] Code is grouped in themed files (modules) and is packaged for easier use. 

B.2.2 Code follows a linear structure:

- [ ] Code logic is clear and avoids unnecessary complexity. 

- [ ] Main analysis scripts import and run high level functions from separate scripts/bespoke package. 

B.2.3 Interdependencies:

- [ ] Where appropriate, map out the interdependencies between scripts/classes/functions. 


### Good Practice 

<details><summary>Click to expand</summary><br />

B.2.1 Consistent convention:

- [ ] Code follows a standard style, potentially using a linter, e.g. [PEP8 for Python](https://www.python.org/dev/peps/pep-0008/) and [Google](https://google.github.io/styleguide/Rguide.html) or [tidyverse](https://style.tidyverse.org/) for R. 

 </details>

### Best Practice 

<details><summary>Click to expand</summary><br />

B.2.1 Consistent convention:

- [ ] Objects and functions are open for extension but closed for modification; functionality can be extended without modifying the source code. 
</details>

</details>

## C. Verification 


_The code should meet the specifications and fulfils its intended purpose_ 

<details><summary>Click to expand</summary>

### Minimum 

C.1 Code matches comments:

- [ ] Comments are kept up to date, so they do not confuse the reader. 

C.2 Reproducibility:

- [ ] Your analysis code is able to reproduce outputs at any time. 

- [ ] Misuse or failure in the code produces informative error messages. 

C.3 Data and assumptions feed in as intended: 

- [ ] Hard coded values in scripts are minimised.

- [ ] Clear route is recorded if decisions based on assumptions/data are made in code. 

C.4 Testing:

- [ ] Code based tests (testing lines of code to identify bugs or errors) are run regularly.  

- [ ] Informal tests are recorded near to the code. 

- [ ] Test code is clean and readable.




### Good Practice 
<details><summary>Click to expand</summary><br />

C.2 Reproducibility:

- [ ] Code configuration is recorded when the code is run. 

C.4 Testing:

- [ ] Core functionality is unit tested as code. See [`pytest` for Python](https://docs.pytest.org/en/stable/) and [`testthat` for R](https://testthat.r-lib.org/).  

- [ ] Tests make use of [fixtures](https://docs.pytest.org/en/7.1.x/explanation/fixtures.html) and parametrisation to reduce repetition. 

- [ ] Bug fixes include implementing new unit tests to ensure that the same bug does not reoccur. 

- [ ] The whole process is tested from start to finish using one or more realistic end-to-end tests. 

- [ ] Formal user acceptance testing (a set of statements with a clear pass/fail results, you should not merge until these are satisfied) is conducted and recorded as per the documentation. 

- [ ] Integration tests (larger than unit tests) ensure that multiple units of code work together as expected. 

 </details> 

### Best Practice 
<details><summary>Click to expand</summary><br />

C.4 Testing:

- [ ] Test are automatically run and recorded using continuous integration or git hooks. 
</details> 

</details> 

## D. Validation 

 

_The correct methodology or approach is being used_ 

<details><summary>Click to expand</summary> 

### Minimum 

D.1 Correct methodology:

- [ ] Reasoning behind methodology or solution is clearly set out in documentation.

- [ ] Explanation is jargon free, explaining key concepts for future developers and users.

D.2 Methodology testing:

- [ ] Appropriate testing has been applied to the solution to validate the outputs (this is separate from unit or integration test which are testing the code itself rather than the outcome). 

D.3 Reproducibility and answering the question:

- [ ] If applicable, stochastic elements influencing the results are clearly explained and suitable ranges/examples are provided. 

- [ ] Evidence the outputs answer the initial question/problem.


### Best Practice 
<details><summary>Click to expand</summary><br />

D.1 Correct methodology and D.2 Methodology testing:

- [ ] Methodology review workshop has been undertaken to stress test the methodology with relevant analysts within the department and/or cross-Government. 
</details> 

</details> 

## E. Data and Assumptions 



_The quality of data, the data flows, ethical considerations, and any assumptions should be clearly set out and stress tested_ 

<details><summary>Click to expand</summary>

### Minimum 

E.1 Data diagram

- [ ] Appropriate diagram showing the flow of data through the pipeline and/or the relationship between different data sources and where these feed into scripts, otherwise documented in text.

E.2 Data ethics

- [ ] Data adheres to the [Government Data Ethics Framework](https://www.gov.uk/government/publications/data-ethics-framework/data-ethics-framework-2020)

E.3 Assumptions clearly stated

- [ ] An assumptions log is completed and saved in a relevant place where the project requires it.

- [ ] There are details on which scripts the assumptions feed into where the project requires it.

E.4 Sensitive data

- [ ] Input data are stored safely and are treated as read-only. 

- [ ] Credentials and other secrets are not written in code but are configured as environment variables.

E.5 Data governance 

- [ ] Large or complex data are stored in a database. 

E.6 Data dictionary and description

- [ ] Fields within input and output datasets are documented in a data dictionary. 

- [ ] All input data and assumptions are documented, including where they come from and their use in the analysis. 

- [ ] Input data are versioned or appropriately timestamped. All changes to the data result in new versions being created, or changes are recorded. 

E.9 Hard-coded values

- [ ] Minimise the number of hard coded values in scripts and, where it is necessarily, make the reasoning clear in the comments or assumptions log.  

### Good Practice 
<details><summary>Click to expand</summary><br />

E.7 Data drift

- [ ] Documentation on the expect “data drift” and how this could impact the final outputs. 

</details> 

### Best Practice 
<details><summary>Click to expand</summary><br />

E.5 Data governance 

- [ ] All data for analysis are stored in an open format, so that specific software is not required to access them. 

- [ ] Non-sensitive data are made available to users. If data are sensitive, dummy data is made available so that the code can be run by others. 

- [ ] Data are documented in an information asset register.

- [ ] Data quality is monitored, as per [the government data quality framework](https://www.gov.uk/government/publications/the-government-data-quality-framework/the-government-data-quality-framework).

</details> 

</details>
