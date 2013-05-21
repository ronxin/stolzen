drop table if exists rain;
create table rain (r char(1) not null, p real);

insert into rain(r, p) values ('y', 0.2);
insert into rain(r, p) values ('n', 0.8);


drop table if exists wet;
create table wet(w char(1), r char(1), p real);

insert into wet(w, r, p) values ('y', 'y', 0.9);
insert into wet(w, r, p) values ('n', 'y', 0.1);
insert into wet(w, r, p) values ('y', 'n', 0.2);
insert into wet(w, r, p) values ('n', 'n', 0.8);


select rain.r as r, sum(wet.p * rain.p) 
	from wet, rain 
	where wet.w = 'y' and wet.r = rain.r 
	group by r;


drop table if exists thunder;
create table thunder(t char(1), r char(1), p real);

insert into thunder(t, r, p) values ('y', 'y', 0.8);
insert into thunder(t, r, p) values ('n', 'y', 0.2);
insert into thunder(t, r, p) values ('y', 'n', 0.1);
insert into thunder(t, r, p) values ('n', 'n', 0.9);


select rain.r as r, sum(wet.p * rain.p * thunder.p) 
	from wet, rain, thunder
	where wet.w = 'y' and thunder.t = 'y' and wet.r = rain.r and rain.r = thunder.r
	group by r;