# V1.0
## user
> ### user_base_info(add) (primary_key: user_id)
> * user_id int required auto_increment
> * login_name varchar(32) required
> * login_pass varchar(64) required
> * user_type int required
> * online_status int required
> * is_valid int required
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
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### user_log_info (add) (primary_key: log_id)
> * log_id int required auto_increment
> * user_id int required
> * log_action int required
> * log_content varchar(512) required
> * log_ip varchar(16) required
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## account
> ### account_user_book_info (add) (primary_key: account_id)
> * account_id int required auto_increment
> * user_id int required
> * account_type int required
> * balance int required
> * bill_id int
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### account_user_book_bill (add) (primary_key: bill_id)
> * bill_id int required auto_increment
> * order_id int required
> * account_id int required
> * user_id int required
> * charge_credit int required
> * charged_before_credit int required
> * charged_after_credit int required
> * charge_status int required
> * charge_date date required
> * charge_time datetime required
> * bill_type int required
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## agent
> ### agent_base_info (add) (primary_key: agent_id)
> * agent_id int required auto_increment

## operate
> ### operate_base_info (add) (primary_key: op_id)
> * op_id int required auto_increment
> * login_name varchar(32) required
> * login_pass varchar(64) required
> * role_id int required
> * online_status int required
> * is_valid int required
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
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### operate_module (add) (primary_key: id)
> * id int required auto_increment
> * module_class varchar(32) required
> * module_action varchar(64) required
> * module_icon varchar(64)
> * parent int required
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### operate_role (add) (primary_key: role_id)
> * role_id int required auto_increment
> * role_name varchar(64) required
> * role_module varchar(128) required
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

## order
> ### order_user_bet_info (add) (primary_key: order_id)
> * order_id int required auto_increment
> * user_id int required
> * game_type int required
> * play_type int required
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### order_user_deposit_info (add) (primary_key: order_id)
> * order_id varchar(64) required
> * user_id int required
> * pre_amount int required
> * amount int
> * third_order_id varchar(64)
> * deposit_time datetime
> * order_status int required
> * deposit_channel int required
> * deposit_ext varchar(128)
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required

> ### order_user_draw_info (add) (primary_key: order_id)
> * order_id varchar(64) required
> * user_id int required
> * draw_amount int required
> * success_amount int
> * third_order_id varchar(64)
> * draw_time datetime
> * order_status int required
> * draw_ext varchar(128) required
> * is_valid int required
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
> * is_valid int required
> * create_time datetime required
> * update_time datetime
> * op_user varchar(32) required
