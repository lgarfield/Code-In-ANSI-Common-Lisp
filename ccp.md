# V1.0
## constant (not table)
> ### is_valid
> * 1: normal
> * 2: frozen
> * 9: del

## account
> ### account_user_book_info (add) (primary_key: account_id)
> * account_id int required auto_increment
> * user_id int required
> * account_type int required
> * balance int required
> * bill_id int
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### account_user_book_bill (add) (primary_key: bill_id)
> * bill_id int required auto_increment
> * order_id int required
> * account_id int required
> * user_id int required
> * charge_credit int required
> * charged_before_credit int
> * charged_after_credit int
> * bill_status int required
> * charge_date date required
> * charge_time datetime required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## agent (TODO)
> ### agent_base_info (add) (primary_key: agent_id)

## dim
> ### dim_bill_status (add) (primary_key: bill_statsu)
> * bill_status int required
> * bill_status_code varchar(32) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
>
> (301, BILL_CREATED)
> (302, BILL_FINISH)

> ### dim_charge_status (add) (primary_key: charge_status)
> * charge_status int required
> * charge_status_code required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
>
> (101, WAIT_CHARGE)
> (102, CHARGING)
> (103, CHARGED)

> ### dim_game_type (add) (primary_key: game_type)
> * game_type int requried
> * game_type_code varchar(32) required
> * table_name varchar(32) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
>
> (201, XY28, xy28)
> ...

> ### dim_order_status (add) (primary_key: order_status)
> * order_status int required
> * order_status_code varchar(32) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
>
> (301, ORDER_CREATED)
> (302, ORDER_FINISH)

> ### dim_order_type (add) (primary_key: order_type)
> * order_type int requird
> * order_type_code varchar(32) requird
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
>
> (101, DEPOSIT)
> (102, WITHDRAW)
> (103, BETTING)
> (104, SAVE_TO_BANK)
> (105, TAKE_FORM_BANK)

> ### dim_lottery_status (add) (primary_key: lottery_status)
> * lottery_status int required
> * lottery_status_code varchar(32) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
>
> (401, WAIT_LOTTERY)
> (402, NO_LOTTERY)
> (403, FINISH_LOTTERY)

> ### dim_account_type (add) (primary_key: account_type)
> * account_type int required
> * account_type_code varchar(32) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
>
> (501, GENERAL)
> (502, BANK)
> ...

> ### dim_user_type (add) (primary_key: user_type)
> * user_type int required
> * user_type_code varchar(32) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
>
> (20, TEST)
> (21, GENERAL)
> (22, ROBOT)

> ### dim_pay_channel (primary_key: pay_channel)
> * pay_channel int required
> * pay_channel_code varchar(32) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
>
> (51, ALIPAY)
> (52, WECHAT)
> (53, JUDZF)
> ...

## extension (TODO)
> ### ext_user_level_info (add) (primary_key: level_id)
> * level_id int required
> * level_code varchar(32) required
> * level_rights varchar(256) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## feed
> ### feed_user_bet_day_data(add) (primary_key: date+agent_id)
> * date date required
> * agent_id int required
> * user_count int required
> * bet_amount int required
> * bet_count int required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## game
> ### game_xy28_play_type (add) (primary_key: play_type)
> * play_type int required auto_increment
> * type_code varchar(64) required
> * type_content varchar(128) required
> * type_odds int required
> * bet_base_amount int required
> * max_bet_count int required
> * min_bet_count int required default 1
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required
>
> 1 大，小，单，双
> 2 小单，小双，大单，大双
> 3 极小值(0-5)，极大值(22-27)
> 4 28个号码定位

> ### game_desc (add) (primary_key: game_type)
> * game_type int required
> * game_code varchar(64) required
> * game_desc varchar(2048) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## operate
> ### operate_base_info (add) (primary_key: op_id)
> * op_id int required auto_increment
> * login_name varchar(32) required
> * login_pass varchar(64) required
> * role_id int required
> * online_status int required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### operate_log_info (add) (primary_key: log_id)
> * log_id int required auto_increment
> * op_id int required
> * log_action int required
> * log_content varchar(512) required
> * log_ip varchar(16) required
> * log_re int required
> * log_remark varchar(128)
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### operate_module (add) (primary_key: id)
> * id int required auto_increment
> * module_class varchar(32) required
> * module_action varchar(64) required
> * parent int required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### operate_role (add) (primary_key: role_id)
> * role_id int required auto_increment
> * role_name varchar(64) required
> * role_module varchar(128) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## order
> ### order_base_info (add) (primary_key: order_id)
> * order_id varchar(64) required
> * user_id int required
> * amount int requird
> * order_status int required
> * order_type int required
> * charge_status int requird
> * charge_date date
> * charge_time datetime
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### order_user_deposit_info (add) (primary_key: id)
> * id int required auto_increment
> * main_order_id varchar(64) required
> * user_id int required
> * pre_amount int required
> * order_status int required
> * amount int
> * third_order_id varchar(64)
> * deposit_time datetime
> * deposit_channel int required
> * deposit_ext varchar(128)
> * deposit_remark varchar(32) // user remark
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### order_user_draw_info (add) (primary_key: id)
> * id int required auto_increment
> * main_order_id varchar(64) required
> * user_id int required
> * draw_amount int required
> * order_status int required
> * success_amount int
> * third_order_id varchar(64)
> * draw_time datetime
> * draw_ext varchar(128) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### order_bet_base_info (add) (primary_key: id)
> * id int required auto_increment
> * main_order_id varchar(64) required
> * game_code varchar(64) required
> * trace_mark boolean required default false
> * trace_number int required default 0
> * lottery_status int required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### order_xy28_info (add) (primary_key: id)
> * id int required auto_increment
> * main_id int required
> * period_number varchar(32) required
> * play_type int required
> * user_id int required
> * bets_count int required
> * single_bet_amount int required
> * total_amount int required
> * bet_odds int required
> * earn_amount int
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## system
> ### sys_config_info (add) (primary_key: config_code)
> * config_code varchar(32) required
> * config_value varchar(128) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### sys_pay_channel_conf (add) (primary:key: conf_id)
> * conf_id int required auto_increment
> * channel_type int required
> * merchant_number varchar(64) required
> * crypt_key varchar(64) requied
> * min_amount int
> * max_amount int
> * amount_list varchar(128)
> * TODO
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## user
> ### user_base_info(add) (primary_key: user_id)
> * user_id int required auto_increment
> * login_name varchar(32) required
> * login_pass varchar(64) required
> * user_type int required
> * online_status int required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### user_ext_info(add) (primary_key: user_id)
> * user_id int required
> * nickname varchar(64) required
> * email varchar(64)
> * mobile varchar(32) required
> * reg_ip varchar(16) required
> * user_security_keys varchar(256)
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### user_log_info (add) (primary_key: log_id)
> * log_id int required auto_increment
> * user_id int required
> * log_action int required
> * log_content varchar(512) required
> * log_ip varchar(16) required
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### user_pay_type_info (add) {primary_key: pay_id}
> * pay_id int required auto_increment
> * user_id int required
> * pay_type int required
> * value varchar(64) required
> * mobile varchar(20)
> * email varchar(64)
> * ext varchar(128)
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### user_passwd_info (add) (primary_key: id)
> * id int required auto_increment
> * user_id int required
> * passwd_code varchar(32) required
> * passwd_info varchar(64) required
> * passwd_remark varchar(64)
> * is_valid int required default 1
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required
