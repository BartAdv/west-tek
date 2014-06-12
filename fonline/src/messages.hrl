-define(NETMSG_LOGIN, 1).
-define(NETMSG_LOGIN_SUCCESS, 2).

-record(msg_add_critter, {crid
			 ,base_type
			 ,hx
			 ,hy
			 ,dir
			 ,condition
			 ,anim1life
			 ,anim1ko
			 ,anim1dead
			 ,anim2life
			 ,anim2ko
			 ,anim2dead
			 ,flags
			 ,multihex
			 ,npc_pid
			 ,npc_dialog_id
			 ,player_name
			 ,parameters}).
