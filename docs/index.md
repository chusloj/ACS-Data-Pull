<!-- TOC depthFrom:1 depthTo:6 withLinks:1 updateOnSave:1 orderedList:0 -->

- [Notes](#notes)
- [Script: filename: `get_data.R`](#script-filename-getdatar)
	- [Setup](#setup)
	- [Package installation](#package-installation)
	- [Setting the working directory](#setting-the-working-directory)
	- [" `parameters/FILEPATHS` "](#-parametersfilepaths-)
	- [" `inputs` "](#-inputs-)
		- [**Special note**: the `name` input](#special-note-the-name-input)
- [Troubleshooting](#troubleshooting)

<!-- /TOC -->

# Notes
- If there's some part of the code that's hopelessly broken, please email me.
- Follow the instructions within the script itself to the letter (please)




# Script: `get_data.R`
This is only documentation for the file `get_data.R` since it's the only file that could have problems when running the code

## Setup
- Make sure that you've pulled the code from github onto your computer and you which directory the folder is in
- Know which folder you have your data/`Excel` files in


## Package installation
Only for the **first time** you run this program, you want to uncomment the lines with
the package installation information, run that line, and then re-comment them
once you've installed the packages. The code looks like this:

```
install.packages(
  "tidyverse",
  "tidycensus",
  "readxl",
  "openxlsx"
)
```


## Setting the working directory
In the toolbar, go to Session -> Set working directory -> **To Source File Location**.
Copy the output and paste it where you see the `setwd` line below `rm(list=ls())`. The code looks like this:

```
rm(list=ls())

setwd("your_directory")
```


<center> You have to do **both steps** in order for this to work (set the working directory manually and then copy it in) </center>




## " `parameters/FILEPATHS` "
Filepath misspecificaiton is what prevented the script from running on windows to begin with.
The key is this stupid feature of `R`:

<center> You need to use a **forward slash** in your directory in the `R` script
if you are using windows, which is the **opposite** of what windows provides
in the document explorer </center>

For example, the output that you get from windows is:

```
setwd("C:\Users\JCHUSL01\Downloads")
```

But what you should put in the R code is:

```
setwd("C:/Users/JCHUSL01/Downloads")
```

This is extremely dumb, but it's how `R` works. Once this is set, everything else
should work just fine.




## " `inputs` "

When customizing the `inputs` section, you can look at
[the documentation for the `tidycensus` package](https://walkerke.github.io/tidycensus/)
for more information.

- When inputting state (`state`), geography (`geo_level`) or county (`cnty`), the names
must appear exactly as they do in the database
  * If the code stops working because `R` says that it "doesn't recognize the geography",
  try changing the case of each word in any of the inputes I've mentioned. You can check
  the [census website](https://data.census.gov/cedsci/) to be sure of the name.

### **Special note**: the `name` input
The `name` variable is used to name your file, but is also used to filter results based on the
geography you choose. Directions for how to use the `name` variable are:

| Geography         | Instructions                                                                                                                                            |   |   |   |
|-------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------|---|---|---|
| State             | Use `st`                                                                                                                                                |   |   |   |
| County            | Use `cnty_name`                                                                                                                                         |   |   |   |
| City or **lower** | For anything lower than city level (including tract, block group, and block) input the name of the city/town/county subdivision that you're focusing on |   |   |   |
|                   |                                                                                                                                                         |   |   |   |


When you pull data at the (usually) tract level or lower, `tidycensus` will pull
data for all cities/towns. `name` works by filtering out _only_ the city that you're looking for.
The code for this part in `loop` is below:

```
input_df <- inner_join(vars,df,by = "variable")
```

If you find that the data has a strange or incorrect output, experiment with the value for
`name` based on the values you get in the original data pull from the `get_acs` function.

That, by the way, is the first thing to appear in the `loop`:

```
  df <- get_acs(geography=geo_level,
              table = tables[[t,1]],
              state = st,
              county = cnty_val,
              cache_table = TRUE,
              year = yr,
              survey = survey_type)
```


# Troubleshooting

Most of the (normal) troubleshooting will have to do with filenames, file paths, and writing the files. In the event that Windows cannot find a file or create a file,

- Make sure that file paths have **forward slashes** `/` in the `R` code
- When running the code to read the excel file with the sheet of tables
within it, make sure:
  1. The end of the filepath has **no slashes** at the end of it
  2. The name of the excel file, which is the `read_file` argument, has **no slashes** at the         beginning of it

When you run the code, the script will automatically put the `data_path` and `read_file` together with this code:

```
labels <- read_excel(paste(data_path,read_file,sep="/"),sheet=var_sheet)
```

That `sep=` argument specifies that character between the `data_path` and the `read_file`, and this separator should always be a forward slash `/` for windows.
