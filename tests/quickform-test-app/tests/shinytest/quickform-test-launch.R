app <- ShinyDriver$new("../../")
app$snapshotInit("quickform-test-launch")

app$setInputs(race = "White")
app$snapshot()
