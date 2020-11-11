app <- ShinyDriver$new("../../")
app$snapshotInit("quickform-test-launch")

app$setInputs(age = 50)
app$snapshot()
