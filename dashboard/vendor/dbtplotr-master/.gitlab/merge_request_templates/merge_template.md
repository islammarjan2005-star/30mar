## Merge Request checklist

The **developer** should specify the checks that they would like the assurer to cover. They should add any additional tick boxes that they would like covered and remove any tick boxes that they don't deem as relevant to this change.

The **assurer** should cover all 5 of these pillars through their QA. They should aim to tick off all the boxes set out. They should include comments and questions as review comments on the GitLab issue so that they are documented. 

## Description 

Developer: please include a summary of changes. *What is this change and its purpose? Is this a bug fix or a feature and does it break any existing functionality? How has it been tested?*

*For example: This MR introduces 2 key changes to the dashboard: (1) shifts data pre-processing from R to SQL and (2) fixes a bug with date strings... These changes have been unit tested and the faster loading times are evidenced in file.ipynb...*

Which categories does this merge request fall into?
- [ ] Bug fix 
- [ ] New feature 
- [ ] Breaking change - *backwards incompatible change, changes expected behaviour*
- [ ] Documentation
- [ ] Structural changes
- [ ] Non-user facing change
 

## Checklist for the developer:

- [ ] I have performed a self-review of my code
- [ ] I have commented my code appropriately, focusing on explaining my design decisions (explain why, not how)
- [ ] I have made corresponding changes to the documentation (comments, docstrings, etc.)
- [ ] I have added tests to evidence that my fix or feature is effective or functional (if applicable)
- [ ] New and existing unit tests pass locally with my changes (if applicable)
- [ ] I have updated the QA log



## Checklist for the assurer:

### 1. Documentation

Any new code includes all the following forms of documentation:

- [ ] **Function Documentation** as docstrings within the function definition.
- [ ] **Examples** demonstrating major functionality, which runs successfully locally.

### 2. Structure and clarity

Any new code is well structured and clear:

- [ ] **Code Comments** enable someone who has not previously seen the code to understand its purpose.
- [ ] **Code Structure** follows a logical progression which is easy to follow.
- [ ] **Functions** are used where appropriate. In particular, there are no repeated large blocks of code.

### 3. Verification

The code produces the intended outputs:

- [ ] **New Code** produces the new functionality/output as expected.
- [ ] **Existing Code** continues to produce the functionality/output as expected.

### 4. Validation

The code produces relevant and valid outputs:

- [ ] **Customers Needs** are met by the new functionality/output.
- [ ] **Consistency** when compared to outputs from any other related analysis.

### 5. Data and Assumptions

The data is handled correctly and assumptions clear:

- [ ] **Data Storage** is in an appropriately secure place.
- [ ] **Data Accessibility** allows users that need to run the code can access the data.
- [ ] **Assumptions** are laid out clearly and can easily be located. Both in the readme/repo and also alongside the outputs. 

## Final approval (post-review)

The author has responded to my review and made changes to my satisfaction.

Estimated time spent reviewing: #

---

## Review comments

These should be kept in the comments of the ticket or merge request.

These might include, but not exclusively:

- Bugs that need fixing. e.g. Does it work as expected? Does it work with other code
  that it is likely to interact with?
- Alternative methods. e.g. Could it be written more efficiently or with more clarity?
- Documentation improvements. e.g. Does the documentation reflect how the code actually works?
- Additional tests that should be implemented. e.g. Do the tests effectively assure that it
  works correctly?
- Code style improvements. e.g. Could the code be written more clearly?

Your suggestions should be tailored to the code that you are reviewing.
Be critical and clear, but not mean. Ask questions and set actions.



*Further reading: [Government Analytical Function guidance on code quality assurance](https://best-practice-and-impact.github.io/qa-of-code-guidance/intro.html)*
