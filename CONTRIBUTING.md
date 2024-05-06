# Contributing to `sl3` development

We, the authors of the `sl3` R package, use the same guide as is used for
contributing to the development of the popular `ggplot2` R package. This
document is simply a formal re-statement of that fact.

The goal of this guide is to help you get up and contributing to `sl3` as
quickly as possible. The guide is divided into two main pieces:

* Filing a bug report or feature request in an issue.
* Suggesting a change via a pull request.

## Issues

When filing an issue, the most important thing is to include a minimal
reproducible example so that we can quickly verify the problem, and then figure
out how to fix it. There are three things you need to include to make your
example reproducible: required packages, data, code.

1.  **Packages** should be loaded at the top of the script, so it's easy to
    see which ones the example needs.

2.  The easiest way to include **data** is to use `dput()` to generate the R
    code to recreate it. For example, to recreate the `mtcars` dataset in R,
    I'd perform the following steps:

       1. Run `dput(mtcars)` in R
       2. Copy the output
       3. In my reproducible script, type `mtcars <- ` then paste.

    But even better is if you can create a `data.frame()` with just a handful
    of rows and columns that still illustrates the problem.

3.  Spend a little bit of time ensuring that your **code** is easy for others to
    read:

    * make sure you've used spaces and your variable names are concise, but
      informative

    * use comments to indicate where your problem lies

    * do your best to remove everything that is not related to the problem.
     The shorter your code is, the easier it is to understand.

You can check you have actually made a reproducible example by starting up a
fresh R session and pasting your script in.

(Unless you've been specifically asked for it, please don't include the output
of `sessionInfo()`.)

## Pull requests

To contribute a change to `sl3`, you follow these steps:

1. Create a branch in git and make your changes.
2. Push branch to github and issue pull request (PR).
3. Discuss the pull request.
4. Iterate until either we accept the PR or decide that it's not a good fit for
   `sl3`.

Each of these steps are described in more detail below. This might feel
overwhelming the first time you get set up, but it gets easier with practice.

If you're not familiar with git or GitHub, please start by reading
<https://r-pkgs.had.co.nz/git.html>

Pull requests will be evaluated against the a checklist:

1.  __Motivation__. Your pull request should clearly and concisely motivates the
   need for change. Plesae describe the problem your PR addresses and show
   how your pull request solves it as concisely as possible.

   Also include this motivation in `NEWS` so that when a new release of
   ggplot2 comes out it's easy for users to see what's changed. Add your
   item at the top of the file and use markdown for formatting. The
   news item should end with `(@yourGithubUsername, #the_issue_number)`.

2.  __Only related changes__. Before you submit your pull request, please
    check to make sure that you haven't accidentally included any unrelated
    changes. These make it harder to see exactly what's changed, and to
    evaluate any unexpected side effects.

    Each PR corresponds to a git branch, so if you expect to submit
    multiple changes make sure to create multiple branches. If you have
    multiple changes that depend on each other, start with the first one
    and don't submit any others until the first one has been processed.

3.  __Use `sl3` coding style__. Please follow the
    [official ggplot2 style](https://adv-r.had.co.nz/Style.html). Maintaing
    a consistent style across the whole code base makes it much easier to
    jump into the code. If you're modifying existing ggplot2 code that
    doesn't follow the style guide, a separate pull request to fix the
    style would be greatly appreciated.

4.  If you're adding new parameters or a new function, you'll also need
    to document them with [roxygen](https://github.com/klutometis/roxygen).
    Make sure to re-run `devtools::document()` on the code before submitting.

This seems like a lot of work but don't worry if your pull request isn't
perfect. It's a learning process. A pull request is a process, and unless
you've submitted a few in the past it's unlikely that your pull request will be
accepted as is. Please don't submit pull requests that change existing
behaviour. Instead, think about how you can add a new feature in a minimally
invasive way.
