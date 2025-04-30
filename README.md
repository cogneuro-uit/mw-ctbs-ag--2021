# R-default

My standard R project setup, using ProjectTemplate. 

ProjectTemplate will
* Reading in the global configuration file contained in `config`.
* Loading any R packages you listed in the configuration file.
* Reading in any datasets stored in `data` or `cache`.
* Preprocessing your data using the files in the `munge` directory.

I have added different folders under "munging" instead of adding them to the src folder.
This is mostly because all these aspects should be reproducible. Thus, one can easily reproduce, *the whole manuscript* by simply running a couple of lines of code. 
This should increase (local) reproducibiltiy of the content, as well as provide clear ways to check all aspects of the code. 
