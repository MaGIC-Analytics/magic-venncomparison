# ─── Tab Visibility Management ────────────────────────────────────────────────

observe({
    hideTab(inputId="NAVTABS", target="Set Comparison")
})

# Show after DE custom-data submit (needs >= 2 files)
observeEvent(input$submit, {
    files <- FileStore()
    if (length(files) >= 2) {
        showTab(inputId="NAVTABS", target="Set Comparison")
        updateTabsetPanel(session, inputId="NAVTABS", selected="Set Comparison")
        shinyjs::delay(300, shinyjs::runjs("$(window).trigger('resize');"))
    } else {
        showNotification("Please upload at least 2 DE results files.", type='warning', duration=5)
    }
})

# Show after DE demo-data submit
observeEvent(input$demo_submit, {
    showTab(inputId="NAVTABS", target="Set Comparison")
    updateTabsetPanel(session, inputId="NAVTABS", selected="Set Comparison")
    shinyjs::delay(300, shinyjs::runjs("$(window).trigger('resize');"))
})

# Show after generic-table submit (needs >= 2 non-empty columns/sets)
observeEvent(input$generic_submit, {
    keys   <- GenericKeys()
    df     <- GenericData()
    keymap <- GenericKeyMap()
    n_sets <- sum(vapply(keys, function(k) length(generic_members(df, keymap[[k]])) > 0,
                         logical(1)))
    if (n_sets >= 2) {
        showTab(inputId="NAVTABS", target="Set Comparison")
        updateTabsetPanel(session, inputId="NAVTABS", selected="Set Comparison")
        shinyjs::delay(300, shinyjs::runjs("$(window).trigger('resize');"))
    } else {
        showNotification("Need at least 2 columns with non-empty values.", type='warning', duration=5)
    }
})
