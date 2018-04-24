app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$uploadFile(file = "test.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(procesar = "click")
# Input '`.clientValue-plotly_relayout-A`' was set, but doesn't have an input binding.
app$snapshot()
