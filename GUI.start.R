# This script builds the Graphical User Interface (GUI)

# First create a window container for the GUI and set the window title
  win = gwindow("Commercial Process Monitoring",height=120, width=320)
  
# Create the main group (holding area) within the window and stack future
# items in the group vertically
  main = ggroup(cont = win, horizontal = FALSE)

# Create the group to house the elements for selecting the CSV file to be used
# add items to the group horizontally
  CSV.group = ggroup(cont = main, horizontal = TRUE)
  # Apply a label in the group
  glabel("CSV file:", cont = CSV.group)
  # Place a text box to the right of the label
  csv.text <- gedit("Select file for upload", cont = CSV.group)   
  # Place a "..." button to the right of the text box - call function 'get.CSV' when the button is pushed
  gbutton("...", handler = get.CSV, cont = CSV.group)

# Create a group under the CSV file group to select the analysis level to be used
  analysis.group = ggroup(cont = main, horizontal = TRUE)
  # Apply a label to the group
  glabel("Analysis level:", cont = analysis.group)
  # Place a drop down with three levels to the right of the label
  analysis.tier <- gdroplist(items=c("Tier 1","Tier 2","Tier 3"), cont = analysis.group)
  # Set the default drop down list item to "Tier 3"
  svalue(analysis.tier) <- "Tier 3"

# Create a group under the Analysis group to set miscellaneous preferences  
  misc.grp = ggroup(cont = main, horizontal = TRUE)
  # Apply a label for the output option
  glabel("Output results to:", cont = misc.grp)
  # Place a drop down with three levels to the right of the label
  analysis.output <- gdroplist(items=c("Screen","PDF","Metafile"), cont = misc.grp)
  # Apply a label for the number of new lots to the right of the drop down
  glabel("Number of new lots:", cont = misc.grp)
  # Place a text box to the right of the label
  # Keep the width small (3 characters) and coerce the input to a numeric
  num.lots <- gedit("", width = 3, coerce.with = as.numeric, cont = misc.grp)

# Place a "spring" to make the next group align to the bottom of the main group
  addSpring(main)

# Create a group for the standard 'okay' and 'cancel' buttons as well as a reset
  button.grp = ggroup(cont = main)
  # Place the reset button at the bottom left
  # call the function 'clear.all' when pressed
  gbutton("Reset all", handler = clear.all, cont = button.grp)
  # Place a spring to make the next items align to the right of the button group
  addSpring(button.grp)
  # Place the continue button then the quit button to the far right bottom
  # When 'continue' is clicked call the function 'process.data'
  gbutton("Continue", handler = process.data, cont = button.grp)
  # when 'cancel' is clicked call the function 'cancel.button'
  gbutton("Quit", handler = cancel.button, cont = button.grp)
