> “*Statistics cannot rescue a poorly designed study.*”
>
> — Douglas Altman

> “*The statistician’s task is not to discover the truth, but to measure uncertainty.*”
>
> — Bradley Efron

# 1 Introduction

You have already seen the basic tidy workflow: import data, inspect it, tidy it, analyse it, and plot it. This chapter focuses on the transformation stage. Here you learn the core `dplyr` verbs well enough that they become routine rather than mysterious.

Transformation in the tidyverse is a small grammar, not a random list of tools. All five verbs share three properties:

1.  The first argument is a data frame.
2.  The output is a data frame.
3.  The result stays tidy unless you *deliberately* collapse it (mainly with `summarise()`).

There are five primary data transformation functions that you will focus on here:

- Arrange observations (rows) with `arrange()`\
- Filter observations (rows) with [`filter()`](https://rdrr.io/r/stats/filter.html)\
- Select variables (columns) with `select()`\
- Create new variables (columns) with `mutate()`\
- Summarise variables (columns) with `summarise()`

> **NOTE:**
>
> - **Need fewer rows?** → [`filter()`](https://rdrr.io/r/stats/filter.html)
> - **Need different row order?** → `arrange()`
> - **Need fewer or re-ordered columns?** → `select()`
> - **Need new columns (same number of rows)?** → `mutate()`
> - **Need fewer rows by aggregating?** → `summarise()` (usually after `group_by()`)

> **TIP:**
>
> - `arrange()` and [`filter()`](https://rdrr.io/r/stats/filter.html) change **rows**, not columns.
> - `select()` and `mutate()` change **columns**, not rows.
> - `summarise()` changes **rows** *on purpose* (it collapses data).

You will use the full South African Coastal Temperature Network dataset for these exercises. Before you begin, however, you will need to cover two new concepts.

``` numberSource
# Load libraries
library(tidyverse)
library(lubridate)

# Load the data from a .RData file
load(here::here("data", "BCB744", "SACTNmonthly_v4.0.RData"))

# Copy the data as a dataframe with a shorter name
SACTN <- SACTNmonthly_v4.0

# Remove the original
rm(SACTNmonthly_v4.0)
```

> **TIP:**
>
> The pipe organises your reasoning. Each line should answer one question: “What rows do I want?” → “What columns do I need?” → “What new columns should I add?” If a pipeline feels confusing, split it into steps and inspect the intermediate outputs.

# 2 Comparison and logical (Boolean) operators

The assignment operator (`<-`) is a symbol that we use to assign some bit of code to an object in your environment. Likewise, **comparison operators** are symbols we use to compare different objects. This is how you tell R how to decide to do many different things. You will see these symbols often out in the “real world” so let us spend a moment now getting to know them better. Most of these should be very familiar to you already:

- `>`: Greater than\
- `>=`: Greater than or equal to
- `<`: Less than
- `<=`: Less than or equal to
- `==`: Equal to
- `!=`: Not equal to

It is important here to note that `==` is for comparisons; `=` is for maths. They are *not* interchangeable, as we may see in the following code chunk. This is one of the more common mistakes one makes when writing code. Luckily the error message this creates should provide us with the clues we need to figure out that we have made this specific mistake.

``` numberSource
SACTN %>% 
  filter(site = "Amanzimtoti")
```

    R> Error in `filter()`:
    R> ! We detected a named input.
    R> ℹ This usually means that you've used `=` instead of `==`.
    R> ℹ Did you mean `site == "Amanzimtoti"`?

> **NOTE:**
>
> `dplyr` inspects your arguments and refuses to guess what you meant. This is a feature: it prevents silent mistakes and makes debugging faster.

The comparison operators are often used together with **Boolean operators**. Boolean operators are used for logical operations and can compare values, resulting in either `TRUE` and `FALSE`. Here they are:

- `!`: NOT - Negates a true value to false, and a false value to true.
- `&`: AND (vectorised) - Returns `TRUE` if both operands are true, `FALSE` otherwise.
- `|`: OR (vectorised) - Returns `TRUE` if at least one of the operands is true.
- `&&`: AND (short-circuit) - Only evaluates the first element of each vector.
- `||`: OR (short-circuit) - Only evaluates the first element of each vector.

Think of `&` as **constraint tightening** (both conditions must be true) and `|` as **constraint loosening** (either condition can be true).

The `%in%` operator in R is a special operator used to test if elements of a vector or data object are contained in another vector or data object. It returns a Boolean vector (`TRUE` and `FALSE`) indicating whether each element of the first vector is found in the second vector. This operator is particularly useful for subsetting or filtering data based on matching values. For example, `x %in% y` will check for each element of `x` if there is a match in `y`, and return a logical vector indicating the presence or absence of each `x` element in `y`.

So, comparison operators are used to make direct comparisons between specific things, but logical operators are used more broadly when making logical arguments. Logic is central to most computing so it is worth taking the time to cover these symbols explicitly here. R makes use of the same *Boolean logic* symbols as many other platforms, including Google, so some (or all) of these will likely be familiar.

When writing a line of tidy code you tend to use these logical operator to combine two or more arguments that use comparison operators. For example, the following code chunk uses the [`filter()`](https://rdrr.io/r/stats/filter.html) function to find all temperatures recorded at Pollock Beach during December *or* January. Do not worry if the following line of code is difficult to piece out, but make sure you can locate which symbols are comparison operators, which are logical operators. Please note that for purposes of brevity all of the outputs in this section are limited to ten lines but when you run these code chunks on your own computer they will be much longer.

``` numberSource
SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1)
```

    R>             site  src       date     temp depth   type
    R> 1  Pollock Beach SAWS 1999-12-01 19.95000     0 thermo
    R> 2  Pollock Beach SAWS 2000-01-01 19.03333     0 thermo
    R> 3  Pollock Beach SAWS 2000-12-01 19.20000     0 thermo
    R> 4  Pollock Beach SAWS 2001-01-01 18.32667     0 thermo
    R> 5  Pollock Beach SAWS 2001-12-01 20.59032     0 thermo
    R> 6  Pollock Beach SAWS 2002-01-01 21.47097     0 thermo
    R> 7  Pollock Beach SAWS 2002-12-01 19.78065     0 thermo
    R> 8  Pollock Beach SAWS 2003-01-01 20.64516     0 thermo
    R> 9  Pollock Beach SAWS 2003-12-01 20.48710     0 thermo
    R> 10 Pollock Beach SAWS 2004-01-01 21.34839     0 thermo

You will look at the interplay between comparison and logical operators in more depth in the following session after you have reacquainted yourself with the main transformation functions you need to know.

# 3 Arrange observations (rows) with `arrange()`

First up in our greatest hits reunion tour is the function `arrange()`. This very simply arranges the observations (rows) in a dataframe based on the variables (columns) it is given. If you are concerned with ties in the ordering of our data you provide additional columns to `arrange()`. The importance of the columns for arranging the rows is given in order from left to right.

When does arranging matter?

- **Checking extremes** (highest/lowest values).
- **Validating temporal order** before plotting.
- **Preparing grouped summaries** so you can inspect patterns.

``` numberSource
SACTN %>% 
  arrange(depth, temp)
```

    R>             site  src       date      temp depth   type
    R> 1      Sea Point SAWS 1990-07-01  9.635484     0 thermo
    R> 2     Muizenberg SAWS 1984-07-01  9.708333     0 thermo
    R> 3     Doringbaai SAWS 2000-12-01  9.772727     0 thermo
    R> 4  Hondeklipbaai SAWS 2003-06-01  9.775000     0 thermo
    R> 5      Sea Point SAWS 1984-06-01 10.000000     0 thermo
    R> 6     Muizenberg SAWS 1992-07-01 10.193548     0 thermo
    R> 7  Hondeklipbaai SAWS 2005-07-01 10.333333     0 thermo
    R> 8  Hondeklipbaai SAWS 2003-07-01 10.340909     0 thermo
    R> 9      Sea Point SAWS 2000-12-01 10.380645     0 thermo
    R> 10    Muizenberg SAWS 1984-08-01 10.387097     0 thermo

If you would rather arrange your data in descending order, as is perhaps more often the case, you simply wrap the column name you are arranging by with the `desc()` function as shown below.

``` numberSource
SACTN %>% 
  arrange(desc(temp))
```

    R>             site   src       date     temp depth type
    R> 1        Sodwana   DEA 2000-02-01 28.34648    18  UTR
    R> 2        Sodwana   DEA 1999-03-01 28.04890    18  UTR
    R> 3        Sodwana   DEA 1998-03-01 27.87781    18  UTR
    R> 4        Sodwana   DEA 1998-02-01 27.76452    18  UTR
    R> 5        Sodwana   DEA 1996-02-01 27.73637    18  UTR
    R> 6        Sodwana   DEA 2000-03-01 27.52637    18  UTR
    R> 7        Sodwana   DEA 2000-01-01 27.52291    18  UTR
    R> 8  Leadsmanshoal EKZNW 2007-02-01 27.48132    10  UTR
    R> 9        Sodwana EKZNW 2005-01-01 27.45619    12  UTR
    R> 10       Sodwana EKZNW 2007-02-01 27.44054    12  UTR

When you arrange data in this way, any rows with `NA` values move to the bottom of the dataframe. Keep that behaviour in mind because it is not always desirable.

# 4 Filter observations (rows) with `filter()`

When simply arranging data is not enough, and you need to remove rows of data you do not want, [`filter()`](https://rdrr.io/r/stats/filter.html) is the tool to use. For example, you can select all monthly temperatures recorded at the `site` Humewood during the `year` 1990 with the following code chunk:

> **WARNING:**
>
> Every [`filter()`](https://rdrr.io/r/stats/filter.html) call removes rows. Stacking multiple filters shrinks your data quickly. If results look “too small,” check each step.

``` numberSource
SACTN %>% 
  filter(site == "Humewood", year(date) == 1990)
```

    R>        site  src       date     temp depth   type
    R> 1  Humewood SAWS 1990-01-01 21.87097     0 thermo
    R> 2  Humewood SAWS 1990-02-01 18.64286     0 thermo
    R> 3  Humewood SAWS 1990-03-01 18.61290     0 thermo
    R> 4  Humewood SAWS 1990-04-01 17.30000     0 thermo
    R> 5  Humewood SAWS 1990-05-01 16.35484     0 thermo
    R> 6  Humewood SAWS 1990-06-01 15.93333     0 thermo
    R> 7  Humewood SAWS 1990-07-01 15.70968     0 thermo
    R> 8  Humewood SAWS 1990-08-01 16.09677     0 thermo
    R> 9  Humewood SAWS 1990-09-01 16.41667     0 thermo
    R> 10 Humewood SAWS 1990-10-01 17.14194     0 thermo

Remember to use the assignment operator (`<-`, keyboard shortcut **alt -**) if you want to create an object in the environment with the new results.

``` numberSource
humewood_90s <- SACTN %>% 
  filter(site == "Humewood", year(date) %in% seq(1990, 1999, 1))
```

[`filter()`](https://rdrr.io/r/stats/filter.html) also removes any rows in the filtering column that contain `NA` values. If you want to keep rows with missing values, include [`is.na()`](https://rdrr.io/r/base/NA.html) in the filtering expression. To illustrate this, let us filter the temperatures for the Port Nolloth data collected by the DEA that were at or below 11°C or were missing values. Each argument is placed on a separate line to keep the logic clear. R automatically indents the last line in this chunk to show that it is part of the same argument. I have also put the final bracket on its own line because that closing bracket is easy to forget.

``` numberSource
SACTN %>% 
  filter(site == "Port Nolloth", # First give the site to filter
         src == "DEA", # Then specify the source
         temp <= 11 | # Temperatures at or below 11°C OR
           is.na(temp) # Include missing values
         )
```

# 5 Select variables (columns) with `select()`

When you load a dataset that contains more columns than will be useful or required, it is preferable to shave off the excess. You do this with the `select()` function. In the following four examples you are going to remove the `depth` and `type` columns. There are many ways to do this and none are technically better or faster. So it is up to the user to find a favourite technique.

> **TIP:**
>
> - **Positive selection**: “What do I want to keep?”
> - **Negative selection**: “What do I want to remove?”
>
> `select()` never changes rows. It only changes columns.

``` numberSource
# Select columns individually by name
SACTN %>% 
  select(site, src, date, temp)

# Select all columns between site and temp like a sequence
SACTN %>% 
  select(site:temp)

# Select all columns except those stated individually
SACTN %>% 
  select(-date, -depth)

# Select all columns except those within a given sequence
  # Note that the '-' goes outside of a new set of brackets
  # that are wrapped around the sequence of columns to remove
SACTN %>% 
  select(-(date:depth))
```

You may also use `select()` to reorder the columns in a dataframe. In this case the inclusion of the `everything()` function may be a useful shortcut as illustrated below.

``` numberSource
# Change up order by specifying individual columns
SACTN %>% 
  select(temp, src, date, site)

# Use the everything function to grab all columns 
# not already specified
SACTN %>% 
  select(type, src, everything())

# Or go bananas and use all of the rules at once
  # Remember, when dealing with tidy data,
  # everything may be interchanged
SACTN %>% 
  select(temp:type, everything(), -src)
```

> **NOTE:**
>
> The square bracket `[]` notation may also be used for indexing and subsetting data structures such as vectors, matrices, data frames, and lists. Before **tidyverse** existed, this was the only way to do so. Square brackets allow you to select elements from these data structures based on their positions, conditions, or names. The use of square brackets can vary slightly depending on the data structure being accessed. Here is a brief overview:
>
> 1.  **Vectors**: When used with vectors, square brackets allow you to select elements by their numeric position, a logical vector indicating which elements to select. For example`vector[c(1, 3)]` returns the first and third elements of the vector.
>
> 2.  **Matrices**: For matrices, square brackets take two dimensions `[row, column]` to select elements. You can select entire rows, columns, or individual elements. Specifying a row, column as empty (*e.g.*, `[,]`) selects everything in that dimension.
>
> 3.  **Dataframes**: Similar to matrices, square brackets can be used to subset data frames by row, column. However and since dataframes can have column names, you can also use these names for selection, *e.g.*, `df[1,]` selects the first row of the dataframe, `df[, "columnName"]` selects all rows of a specific column.
>
> 4.  **Lists**: Lists can be subsetted by numeric or character indices corresponding to their elements. For example, `list[[1]]` selects the first element of the list. Note the double brackets, which are used to extract elements from a list directly. Single brackets, *e.g.*, `list[1]`, return a sublist containing the first element.

> **NOTE:**
>
> Base R subsetting is powerful but low-level. Tidyverse verbs are higher-level: they make your intent explicit and read like a sentence. Use the one that makes your reasoning clearest.

> **IMPORTANT:**
>
> Do this now: provide examples of i) the various data structures available in R, and ii) how to use square brackets to subset each of them. You may use any of the built-in datasets to do so.

# 6 Create new variables (columns) with `mutate()`

When you are performing data analysis/statistics in R this is likely because it is necessary to create some new values that did not exist in the raw data. The previous three functions you looked at (`arrange()`, [`filter()`](https://rdrr.io/r/stats/filter.html), `select()`) will prepare you to create new data, but do not do so themselves. This is when you need to use `mutate()`. You must however be very mindful that `mutate()` is only useful if we want to create new variables (columns) that are a function of one or more *existing* columns (well, that is how it is mainly used). Any new column you create with `mutate()` must always have the same number of rows as the dataframe you are working with. In order to create a new column you must first tell R what the name of the column will be, in this case let us create a column named `kelvin`. The second step is to then tell R what to put in the new column. As you may have guessed, you are going to convert the `temp` column which contains degrees Celsius (C) into Kelvin (K) by adding 273.15 to every row.

> **WARNING:**
>
> - Trying to create a *summary* (fewer rows) inside `mutate()` → use `summarise()` instead.
> - Accidentally **overwriting** an existing column when you meant to create a new one.

``` numberSource
SACTN %>% 
  mutate(kelvin = temp + 273.15)
```

    R>            site src       date     temp depth type   kelvin
    R> 1  Port Nolloth DEA 1991-02-01 11.47029     5  UTR 284.6203
    R> 2  Port Nolloth DEA 1991-03-01 11.99409     5  UTR 285.1441
    R> 3  Port Nolloth DEA 1991-04-01 11.95556     5  UTR 285.1056
    R> 4  Port Nolloth DEA 1991-05-01 11.86183     5  UTR 285.0118
    R> 5  Port Nolloth DEA 1991-06-01 12.20722     5  UTR 285.3572
    R> 6  Port Nolloth DEA 1991-07-01 12.53810     5  UTR 285.6881
    R> 7  Port Nolloth DEA 1991-08-01 11.25202     5  UTR 284.4020
    R> 8  Port Nolloth DEA 1991-09-01 11.29208     5  UTR 284.4421
    R> 9  Port Nolloth DEA 1991-10-01 11.37661     5  UTR 284.5266
    R> 10 Port Nolloth DEA 1991-11-01 10.98208     5  UTR 284.1321

This is a very basic example; `mutate()` is capable of much more than simple addition. You will get into some more exciting examples during the next session.

# 7 Summarise variables (columns) with `summarise()`

Finally this brings you to the last tool for this section. To create new columns you use `mutate()`, but to calculate any sort of summary/statistic from a column that will return fewer rows than the dataframe has you will use `summarise()`. This makes `summarise()` much more powerful than the other functions in this section, but because it is able to do more, it can also be more unpredictable, making its use potentially more challenging. You will almost always end up using this function in our workflows. The following chunk very simply calculates the overall mean temperature for the entire SACTN.

Think of `summarise()` as a **controlled rupture**: it intentionally breaks the one-observation-per-row structure to produce aggregate values. This is why it is almost always paired with `group_by()` later.

``` numberSource
SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE))
```

    R>   mean_temp
    R> 1  19.26955

Note how the above chunk created a new dataframe. This is done because it cannot add this one result to the previous dataframe due to the mismatch in the number of rows. If you were to create additional columns with other summaries, you may do so within the same `summarise()` function. These multiple summaries are displayed on individual lines in the following chunk to help keep things clear.

``` numberSource
SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE)
            )
```

    R>   mean_temp  sd_temp min_temp max_temp
    R> 1  19.26955 3.682122 9.136322 28.34648

Creating summaries of the *entire* SACTN dataset in this way is not appropriate as you should not be combining time series from such different parts of the coast. In order to calculate summaries within variables you will need to learn how to use `group_by()`, which in turn will first require you to learn how to chain multiple functions together within a pipe (`%>%`). That is how you will begin the next session for today. You will finish with several tips on how to make your data the tidiest that it may be.

> **TIP:**
>
> If a result looks wrong:
>
> - Run the pipeline stepwise and inspect intermediate objects.
> - Check row counts before and after each verb.
> - Use `count()` or `summarise(n = n())` to confirm expectations.

## Reuse

[CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/)

## Citation

BibTeX citation:

``` quarto-appendix-bibtex
@online{smit2021,
  author = {Smit, A. J.},
  title = {14. {Tidier} Data},
  date = {2021-01-01},
  url = {https://tangledbank.netlify.app/BCB744/intro_r/14-tidier.html},
  langid = {en}
}
```

For attribution, please cite this work as:

Smit AJ (2021) 14. Tidier data. [https://tangledbank.netlify.app/BCB744/intro_r/14-tidier.html.](https://tangledbank.netlify.app/BCB744/intro_r/14-tidier.html)
