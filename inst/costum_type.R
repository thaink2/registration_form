
shinysurveys::extendInputType(input_type = "slider", {
  polished::email_input(
    inputId = surveyID(),
    label = surveyLabel(),
    value = "",
    width = NULL,
    placeholder = NULL
  )
})


mailR::send.mail(from = "an.meziani@gmail.com",
                 to = c( "faridazouaou@gmail.com"),
                 #cc = c("CC Recipient <cc.recipient@gmail.com>"),
                 #bcc = c("BCC Recipient <bcc.recipient@gmail.com>"),
                 subject = "Training booking",
                 body = "Candidate Infos",
                 smtp = list(host.name = "aspmx.l.google.com", port = 25),
                 authenticate = FALSE,
                 send = TRUE)
