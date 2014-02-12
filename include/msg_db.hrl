-record(msg_db, {id,			%int()
				topic,			%atom()
				timestamp,		%{int(),int(),int()}
				data			%binary()
				}).