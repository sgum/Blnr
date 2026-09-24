# ui/login_ui.R
#
# Типовая форма авторизации ЦД (dt-auth-skill, references/form.md).
# Стенд закрыт AD + белым списком (только s.gumerov/v.alad, без self-service
# регистрации) — поэтому из стандартного блока ссылок оставлена только
# «Забыли пароль?» (ведёт на контакт ИТ), а «Регистрация» сознательно
# опущена: регистрироваться в этот стенд некому.

loginUI <- function(stand_title = "Портфель — broker.dtwin.ru") {
  tags$div(
    class = "dt-auth-wrap",
    tags$div(
      class = "dt-auth-card",
      tags$img(class = "dt-auth-logo", src = "dt-auth-logo-ru.png", alt = "Цифровой Двойник"),
      tags$div(class = "dt-auth-brand", stand_title),
      tags$div(
        class = "dt-auth-title-row",
        tags$h2("Вход в стенд"),
        tags$span(
          class = "dt-auth-info", tabindex = "0", role = "note", `aria-label` = "Подсказка",
          "i",
          tags$span(
            class = "dt-auth-tip",
            "Сотрудники ЦД входят доменной учётной записью — тем же логином и паролем, ",
            "что и на рабочем компьютере. Доступ к стенду — только по списку."
          )
        )
      ),
      textInput("auth_login", label = NULL, placeholder = "Email или доменный логин"),
      tags$div(
        class = "dt-auth-pw",
        passwordInput("auth_password", label = NULL, placeholder = "Пароль"),
        tags$button(id = "auth_eye", type = "button", class = "dt-auth-eye",
                    title = "Показать пароль",
                    onclick = "(function(){var p=document.getElementById('auth_password'); if(p){p.type = p.type==='password' ? 'text' : 'password';}})()",
                    HTML("&#128065;"))
      ),
      actionButton("auth_submit", "Войти", class = "dt-auth-btn", width = "100%"),
      tags$p(class = "dt-auth-switch",
             tags$a(href = "mailto:support@dtwin.ru", "Забыли пароль?")),
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
    /* Шрифт формы — ТОЛЬКО Panton, без запасных вариантов (решение СГ
       19.09.2026, dt-auth-skill/references/form.md): явный провал шрифта
       лучше тихой подмены на системный. */
    .dt-auth-wrap{min-height:100vh;display:flex;align-items:center;justify-content:center;
      background:#ECE9E2;font-family:Panton;}
    .dt-auth-card{background:#fff;border-radius:12px;padding:28px 32px;width:360px;max-width:92vw;
      box-shadow:0 8px 30px rgba(0,0,0,.12);}
    .dt-auth-logo{display:block;width:min(250px,100%);height:76px;margin:0 auto 12px;object-fit:contain;}
    .dt-auth-brand{font-size:23px;font-weight:700;letter-spacing:-.2px;line-height:1.25;
      text-align:center;color:#15120F;}
    .dt-auth-title-row{display:flex;align-items:center;justify-content:center;gap:6px;margin:10px 0 18px;}
    .dt-auth-title-row h2{margin:0;font-size:13px;font-weight:400;color:#8a8a8a;font-family:Panton;}
    .dt-auth-info{position:relative;flex:none;display:inline-flex;align-items:center;justify-content:center;
      width:16px;height:16px;border-radius:50%;background:#eee;color:#8a8a8a;
      font:700 11px Panton;line-height:1;cursor:default;}
    .dt-auth-info:hover,.dt-auth-info:focus-visible{background:#b26a00;color:#fff;outline:none;}
    .dt-auth-tip{position:absolute;left:50%;transform:translateX(-50%);top:22px;z-index:5;
      width:230px;max-width:60vw;background:#15120F;color:#fff;font:400 11.5px/1.45 Panton;
      padding:8px 10px;border-radius:8px;box-shadow:0 6px 20px rgba(0,0,0,.35);
      opacity:0;visibility:hidden;transition:opacity .12s;text-align:left;}
    .dt-auth-info:hover .dt-auth-tip,.dt-auth-info:focus-visible .dt-auth-tip{opacity:1;visibility:visible;}
    .dt-auth-pw{position:relative;}
    .dt-auth-eye{position:absolute;right:6px;top:6px;border:none;background:none;cursor:pointer;
      padding:4px 6px;font-size:15px;line-height:1;opacity:.45;border-radius:6px;}
    .dt-auth-eye:hover{opacity:.85;}
    .dt-auth-btn{background:orange;border-color:orange;color:#fff;font-weight:700;margin-top:6px;
      font-family:Panton;}
    .dt-auth-switch{text-align:center;margin-top:14px;font-size:13px;color:#8a8a8a;font-family:Panton;}
    .dt-auth-switch a{color:#b26a00;text-decoration:none;font-weight:600;}
    .dt-auth-card .form-group{margin-bottom:14px;}
    .dt-auth-card input.form-control{font-size:13px;padding:8px 34px 8px 10px;height:auto;
      font-family:Panton;}
    .dt-auth-card .shiny-label-null{display:none;}
    .dt-auth-err{color:#c62828;font-size:12px;margin-top:12px;text-align:center;font-family:Panton;}
  "))
}
