drop table if exists splinkler;
create table splinkler (s char(1), p real);

insert into splinkler(s, p) values ('y', 0.3);
insert into splinkler(s, p) values ('n', 0.7);


drop table if exists rain;
create table rain (r char(1), p real);

insert into rain(r, p) values ('y', 0.2);
insert into rain(r, p) values ('n', 0.8);


drop table if exists wet;
create table wet(w char(1), s char(1), r char(1), p real);

insert into wet(w, s, r, p) values ('y', 'y', 'y', 0.9);
insert into wet(w, s, r, p) values ('y', 'y', 'n', 0.7);
insert into wet(w, s, r, p) values ('y', 'n', 'y', 0.8);
insert into wet(w, s, r, p) values ('y', 'n', 'n', 0.1);
insert into wet(w, s, r, p) values ('n', 'y', 'y', 0.9);
insert into wet(w, s, r, p) values ('n', 'y', 'n', 0.2);
insert into wet(w, s, r, p) values ('n', 'n', 'y', 0.3);
insert into wet(w, s, r, p) values ('n', 'n', 'n', 0.1);


select  wet.w as w, wet.p,
		splinkler.s as s, splinkler.p,
		rain.r as r, rain.p,
		splinkler.p * wet.p * rain.p
	from splinkler, wet, rain
	where wet.w = 'y' and wet.r = rain.r and wet.s = splinkler.s;

-- given evidence 'grass is wet', i.e. w = 'y'
select wet.w as w, rain.r as r, sum(splinkler.p * wet.p * rain.p)
	from splinkler, wet, rain
	where wet.w = 'y' and wet.r = rain.r and wet.s = splinkler.s
	group by r;


select  wet.w as w, wet.p,
		splinkler.s as s, splinkler.p,
		rain.r as r, rain.p,
		splinkler.p * wet.p * rain.p
	from splinkler, wet, rain
	where wet.w = 'y' and splinkler.s = 'y' and wet.s = splinkler.s and wet.r = rain.r
	group by r;

-- given evidence 'grass is wet' and 'sprinkler is on', i.e. w = 'y' and s = 'y'
select  wet.w as w,
		rain.r as r,
		sum(splinkler.p * wet.p * rain.p)
	from splinkler, wet, rain
	where wet.w = 'y' and splinkler.s = 'y' and wet.s = splinkler.s and wet.r = rain.r
	group by r;