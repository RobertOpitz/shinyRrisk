# shinyRrisk
App for stochastic, quantitative risk assessment using R and Shiny. This app runs in your web browser.

# Download

The following describes how to download the software directly from this github page, or from the command line. 

## Download directly from this github page

Click on `Code` to open this pull down menue.

![download_shinyrrisk_step1](https://user-images.githubusercontent.com/52924688/146906248-b50a23d9-0c04-4052-a3f8-e70a8a07779f.png)

Click on `Download Zip` to download shinyRrisk as a compressed .zip file.

![download_shinyrrisk_step2](https://user-images.githubusercontent.com/52924688/146906252-b2cd26aa-4c71-454d-804a-c4391ea66586.png)

Unzip this file in the directory of your choice.

## Download from github from command line

From the command line use the following in the directory of your choice:

```console
git clone https://github.com/RobertOpitz/shinyRrisk.git
```

# Starting shinyRrisk

The following describes how to start `shinyRrisk` from RStudio, or from the command line.

## Starting shinyRrisk with RStudio

The following describes how to start `shinyRrisk` from RStudio directly. If you do not have all necessary packages for `shinyRrisk`, it may install the missing packages for you. Then, you should restart R either by choosing from the menue `Session -> Restart R`, or by closing RStudio and opening it again. 

 First go to the shinyRrisk directory. You can use in RStudio `...` for this.
 
![RStudio_browse_to_directory_painted_get_file](https://user-images.githubusercontent.com/52924688/146752468-1f2c8619-25e3-422d-85c4-11c48e039af2.png)

Next you need to set the shinyRrsik directory as the working directory. You can click on the gear for that.

![RStudio_browse_to_directory_painted_open_click_on_more](https://user-images.githubusercontent.com/52924688/146752123-d7b7ccdf-6bf0-457f-9574-6a740080d50f.png)

Then select the entry `Set As Working Directory`.

![RStudio_browse_to_directory_set_working_directory](https://user-images.githubusercontent.com/52924688/146752646-99a20f97-3217-4b40-9e0e-e5148a18b37e.png)

 You can set it also in the R console with `setwd`.
 
![RStudio_browse_to_directory_set_working_directory_2](https://user-images.githubusercontent.com/52924688/146752162-569cdfbd-f4fc-4ebc-94f6-571ce2500f6d.png)

Click on shinyRrisk.R to load the script.

![RStudio_browse_to_directory_painted_open_shinyRrisk](https://user-images.githubusercontent.com/52924688/146752418-8a08369a-3cd6-44a7-82ca-9c488a73fec6.png)

Start the App.

![RStudio_browse_to_directory](https://user-images.githubusercontent.com/52924688/146752053-3dd334dd-81f8-47a8-a974-481d604fd35f.PNG)

## Starting shinyRrisk from command line

You may start shinyRrisk from the command line using the program `Rscript` that is part of R. Go into the shinyRrisk folder, and use the following command: 

```console
Rscript shinyRrisk.R
```
If your R does not have all of the necessary packages needed for shinyRrisk, they will be downloaded and installed by shinyRrisk. **Then, you may call `shinyRrisk.R` twice using `Rscript`.** 
