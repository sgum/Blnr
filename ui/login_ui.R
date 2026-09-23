# ui/login_ui.R
#
# Типовая форма авторизации ЦД (упрощённая под стенд на 2 человек):
# одно поле «Email или доменный логин» + пароль, кнопка «Войти», ошибка —
# строкой под формой, подсказка — под значком «i» у заголовка.
# Разметка/тексты — dt-auth-skill (references/form.md).

loginUI <- function(stand_title = "Портфель — broker.dtwin.ru") {
  tags$div(
    class = "dt-auth-wrap",
    tags$div(
      class = "dt-auth-card",
      tags$div(class = "dt-auth-brand", "ЦИФРОВОЙ ДВОЙНИК"),
      tags$div(
        class = "dt-auth-title-row",
        tags$span(class = "dt-auth-title", stand_title),
        tags$span(
          class = "dt-auth-info", `data-toggle` = "tooltip",
          title = paste("Сотрудники ЦД входят доменной учётной записью",
                        "(логин вида s.gumerov или почта @dtwin.ru).",
                        "Доступ к стенду — только по списку."),
          "i"
        )
      ),
      textInput("auth_login", label = NULL, placeholder = "Email или доменный логин"),
      tags$div(
        class = "dt-auth-pw",
        passwordInput("auth_password", label = NULL, placeholder = "Пароль"),
        tags$button(id = "auth_eye", type = "button", class = "dt-auth-eye",
                    onclick = "(function(){var p=document.getElementById('auth_password'); if(p){p.type = p.type==='password' ? 'text' : 'password';}})()",
                    "\U0001F441")
      ),
      actionButton("auth_submit", "Войти", class = "dt-auth-btn", width = "100%"),
      uiOutput("login_error")
    ),
    tags$script(HTML(
      "document.addEventListener('keydown',function(e){",
      "if(e.key==='Enter'){var b=document.getElementById('auth_submit'); if(b) b.click();}});"
    ))
  )
}

# CSS формы — отдельной функцией, включается в шлюз ui.R.
loginCSS <- function() {
  tags$style(HTML("
    .dt-auth-wrap{min-height:100vh;display:flex;align-items:center;justify-content:center;
      background:#ECE9E2;font-family:'Panton',sans-serif;}
    .dt-auth-card{background:#fff;border-radius:10px;padding:34px 30px;width:360px;max-width:92vw;
      box-shadow:0 8px 30px rgba(0,0,0,.12);}
    .dt-auth-brand{font-size:11px;letter-spacing:2px;color:#b26a00;font-weight:700;text-align:center;margin-bottom:6px;}
    .dt-auth-title-row{display:flex;align-items:center;justify-content:center;gap:8px;margin-bottom:22px;}
    .dt-auth-title{font-size:18px;font-weight:700;color:#15120F;text-align:center;}
    .dt-auth-info{display:inline-flex;align-items:center;justify-content:center;width:18px;height:18px;
      border-radius:50%;border:1px solid #b26a00;color:#b26a00;font-size:11px;font-style:italic;cursor:help;flex:0 0 auto;}
    .dt-auth-pw{position:relative;}
    .dt-auth-eye{position:absolute;right:8px;top:6px;border:none;background:transparent;cursor:pointer;font-size:15px;line-height:1;}
    .dt-auth-btn{background:orange;border-color:orange;color:#fff;font-weight:700;margin-top:6px;}
    .dt-auth-card .form-group{margin-bottom:14px;}
    .dt-auth-card input.form-control{font-size:13px;padding:8px 10px;height:auto;}
    .dt-auth-err{color:#c62828;font-size:12px;margin-top:12px;text-align:center;}
  "))
}
