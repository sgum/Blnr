# Деплой Blnr на broker.dtwin.ru

Стенд мониторинга портфеля с историей. **Финансовые данные — доступ только по
белому списку** (`BLNR_ALLOWED_USERS`, по умолчанию `s.gumerov`, `v.alad`).

## Почему установка ручная

Репозиторий личный (`sgum/Blnr`). Корпоративный `dt-deploy`/офис-бот клонирует
только из организации `St-Digital-Twin`, поэтому автоматический путь для этого
репозитория недоступен. Установку на Petr выполняет человек с доступом (SSH к
`srv-dgt-petr`, shiny-server, nginx01, Cloudflare). Ниже — что именно сделать.

Инфраструктура (из `dt-deploy get_infra_map`): приложения — `srv-dgt-petr`
(`10.4.234.253`), публичный reverse-proxy + Let's Encrypt — `srv-dgt-nginx01`
(`109.172.113.252`), shiny-server порт `3838`, зона Cloudflare `dtwin.ru`.

## 0. Реестр стендов

Перед публикацией добавить строку в **DT.1359 → «2. PUBLIC Services»**
(домен, назначение, состояние). Стенд закрыт — отметить способ авторизации.

## 1. Пакеты и утилиты на Petr

```bash
sudo apt install ldap-utils            # ldapwhoami для AD-bind
sudo -u shiny Rscript -e 'install.packages(c(
  "shiny","bs4Dash","fresh","shinyWidgets","shinyjs","rhandsontable",
  "quantmod","plotly","data.table","openxlsx","httr","jsonlite","jose","bcrypt"))'
# CA-сертификат домена ЦД положить на сервер и указать в AD_CA_CERT (.Renviron)
```

Локаль shiny-server должна быть **UTF-8** (иначе кириллица в интерфейсе
превращается в байты `<d0>…`): `LANG=ru_RU.UTF-8`/`en_US.UTF-8` в окружении
сервиса (`/etc/shiny-server.conf` → `env` или systemd drop-in).

## 2. Код и рантайм-конфиг

```bash
# каталог приложения (пример)
sudo -u shiny git clone https://github.com/sgum/Blnr.git /srv/shiny/broker
cd /srv/shiny/broker
```

Создать `.Renviron` рядом с `global.R` (см. `.Renviron.example`), заполнить:

- `EXANTE_API_ID`, `EXANTE_SHARED_KEY` — **боевой ключ вводится прямо на сервере**,
  не через git/чат/Telegram;
- `BLNR_ALLOWED_USERS=s.gumerov,v.alad`;
- `AD_ENABLED=TRUE` + блок `AD_*` (стандартные значения ЦД в примере) + `AD_CA_CERT`;
- `FORECAST_XLSX_PATH` — путь к файлу прогноза на сервере;
- `BLNR_SNAPSHOT_LOG=/mnt/data-external/Blnr-data/portfolio_snapshots.csv`
  (**постоянный том вне git-checkout** — иначе история теряется при каждом деплое);
- при необходимости резерва — `BLNR_LOCAL_USERS` (bcrypt-хеш, не открытый текст).

Права на каталог истории: `sudo install -d -o shiny -g shiny /mnt/data-external/Blnr-data`.

## 3. Авторизация

Встроена в приложение (`R/auth_ad.R` + `ui/login_ui.R`): до входа отдаётся форма,
дашборд и его данные в браузер не уходят. Проверка — AD (LDAPS simple bind под
самим пользователем, две формы имени `s.gumerov@ad.dtwin.ru` и `AD\s.gumerov`),
затем локальный bcrypt-резерв; поверх — белый список. Пустой пароль отклоняется
до обращения к домену. Ошибка не различает «нет пользователя» и «неверный пароль».

Этого достаточно для shiny-server. Если стенд ставится за общий nginx-шлюз
платформы (`auth_request` к `/auth/me`, как у 200.ТЭБ/227) — закрыть **и** `location /`,
**и** websocket/`/session`, иначе данные достаются мимо входа (см. dt-auth
`references/method.md`).

## 4. shiny-server + публикация

- Прописать сайт в shiny-server (location → `/srv/shiny/broker`, порт 3838).
- На `nginx01`: vhost `broker.dtwin.ru` → proxy на shiny Petr, Let's Encrypt
  (`support@dtwin.ru`), websocket-upgrade проксировать.
- Cloudflare: A-запись `broker.dtwin.ru` → `109.172.113.252` (зона `dtwin.ru`).
- Перечитать: `sudo -u shiny touch /srv/shiny/broker/restart.txt`.

## 5. История портфеля — ежедневный снимок (обязательно)

История копится скриптом `scripts/snapshot.R`, запускаемым **по расписанию**, а не
только при открытии вкладки (конституция: актуализация — заданием). Он считает
факт/прогноз и дописывает строку в `BLNR_SNAPSHOT_LOG` (идемпотентно по дате),
код возврата ≠ 0 при пустом результате.

systemd-timer (пример, ежедневно 23:30):

```ini
# /etc/systemd/system/blnr-snapshot.service
[Service]
Type=oneshot
User=shiny
WorkingDirectory=/srv/shiny/broker
Environment=LANG=en_US.UTF-8
ExecStart=/usr/bin/Rscript scripts/snapshot.R
```
```ini
# /etc/systemd/system/blnr-snapshot.timer
[Timer]
OnCalendar=*-*-* 23:30:00
Persistent=true
[Install]
WantedBy=timers.target
```
```bash
sudo systemctl enable --now blnr-snapshot.timer
```

(Cron — рабочий вариант, но у него нет истории прогонов; timer/Jenkins предпочтительнее.)

## 6. Проверка после включения

```bash
curl -s -o /dev/null -w '%{http_code}\n' https://broker.dtwin.ru/     # 200, отдаётся форма входа
```
Вручную: форма входа появляется; `s.gumerov` с доменным паролем входит; чужой
доменный логин (не из белого списка) — не входит; после входа виден дашборд и в
шапке «Выйти»; на вкладке «Портфель Exante» — карточка «Накопление невязки
прогноза во времени» (наполняется со второго снимка).

## 7. Прод только после теста

По политике P.035/ЦД.2860 сначала тестовый контур `test.broker.dtwin.ru`,
прод `broker.dtwin.ru` — отдельным решением. Личные ветки на общий стенд не
публикуются: держать деплой-ветку (`main` или `test`).
