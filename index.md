# Preface

This book has been written to accompany the course, *BB852 - Data
Handling, Visualisation and Statistics*.

It is available as a website (<https://jonesor.github.io/BB852_Book/>)
or as a PDF (click the link at the top of book’s website). I recommend
to use the website where possible because the formatting is sometimes
messy on the PDF, but the PDF is useful if you want a copy for offline
use.

<p>
<strong>Note:</strong> The book is a “work in progress” and will change
during the course. The latest version can always be found at the
website, or by downloading it again. Please let me know (<a
href="mailto:jones@biology.sdu.dk">jones@biology.sdu.dk</a>) if you spot
any errors, or have any suggestions for improvement.
</p>

The book is divided into three parts: data wrangling, data visualisation
and statistics.

## Data wrangling

The term data wrangling covers manipulation of data, for example
collected from an experiment or observational study, from its raw form
to a form that is ready for analysis, or summarised into tables. It
includes reshaping, transforming, filtering and augmenting from other
data. This book covers these processes in R mainly using the tools from
the `dplyr` package.

## Data visualisation

Graphing data is a crucial analytical step that can both highlight
problems with the data (e.g. errors and outliers) and can inform on
appropriate statistical approaches to take. This book covers the use of
`ggplot2` to make high quality, publication-ready plots.

## Statistics

Statistics is a *huge* field and this book does not attempt to cover
more than a small fraction of it. Instead it focusses on (ordinary)
linear models and generalised linear models. In a nutshell, linear
models model the effects of explanatory variables on a continuous
response variable with a gaussian (normal) error distribution while
generalised linear models (GLMs) offer a more flexible approach that
allows the response variable to have non-normal error distributions.
This flexibility allows the more-appropriate modelling of phenomena
including integer counts (e.g. number of individuals, or species, or
events), binary (0/1) data (e.g. survived/died) or binomial count data
(e.g. counts of successes and failures). It is important to realise that
most commonly-used statistical methods including t-tests, ANOVA, ANCOVA,
n-way ANOVA, and of course linear and multiple regression are all
special cases of linear models.

My general approach with communicating these methods and ideas is to
teach using examples. Therefore, the bulk of the text here consists of
walk-throughs of manipulating, plotting and analysing real data. For the
statistics section I focus on communicating the “gist” of the underlying
mathematical machinery rather than the mathematical details. If you find
yourself interested in these details then there are more specialist
textbooks available.

<p>
This book accompanies the course lectures. The general idea is that
there will be a lecture, followed by computer work where you work
through the examples in the relevant chapter of this book. At the end of
most chapters there are also exercises to test your new skills. It is
very important that you do these to gradually build up your skill level
and confidence.
</p>

## Data sources

This book uses numerous data sets in examples, most of which are real
data sets obtained from published works, or collected by me.

The data sets can be found at the following link:
<https://www.dropbox.com/sh/m8qjm1v7c3bunjg/AACyID24e9VjBflhPzPaN6nza?dl=0>

## Your instructors

You are welcome to contact instructors with any problems/questions (but
please put a little effort in first).

-   [Owen Jones](https://portal.findresearcher.sdu.dk/en/persons/jones),
    Associate Professor, <jones@biology.sdu.dk>
-   X
-   X

## Expectations

There are lectures and practical exercise sessions on the course. The
exercise sessions are essential to understand the subject and I expect
students to attend and actively participate in them. I also expect
students to make every effort to keep up with the core reading (mainly
the textbook chapters), and to ask questions where they don’t
understand. The nature of the course is that it builds sequentially and
therefore, if you miss classes or fall behind, it may be hard to catch
up. If you don’t finish off exercises in class time you should finish
them as homework.

## Your feedback

I aim to make this course useful and rewarding for you. I would really
like your feedback on how the course is progressing so I can address any
issues that come up as soon as possible. To help with this I have
created a simple Google Form: ​<https://forms.gle/wZhUfy35ZxEmomYt6>​. You
can use this to send me (Owen) comments (anonymously if you wish) at any
time in the course. I promise to do my best to resolve any problems.

## Assessment

Your final grade will be determined by a multiple choice question (MCQ)
exam, worth 30% of your grade, and a written take home exam, worth 70%
of your grade. The take home will include several questions where you
apply your new skills to analyse some interesting data and report your
findings.

## Acknowledgements

These materials are inspired by the excellent textbook, “Getting Started
With R”[1], which is the recommended textbook for BB852, and by
materials for the Sheffield University course “AP 240 - Data Analysis
And Statistics With R” (<https://dzchilds.github.io/stats-for-bio/>).
For your convenience, the data sets for the Beckerman et al. book are
available at this course’s data Dropbox link (above), in a folder called
“GSWR\_datasets”.

[1] Beckerman, Childs & Petchey (2017) *Getting Started With R*. Oxford
University Press (2nd edition)
