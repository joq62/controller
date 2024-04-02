all:
	rm -rf  *~ */*~ src/*.beam test/*.beam test_ebin erl_cra*;
	rm -rf *_a;
	rm -rf *.dir;
	rm -rf _build;
	rm -rf logs;
	rm -rf ebin
	rm -rf rebar.lock;
#	mkdir ebin;		
	rebar3 compile;	
#	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
#	git add -f *;
	git add *;
	git commit -m $(m);
	git push;
	echo Ok there you go!
no_ebin_commit:
	#INFO: no_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf inventory;
	rm -rf catalog application_dir deployment_specs catalog_specs;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	rm -rf ebin;
	rebar3 edoc;
#	git add *;
#	git add -f *;
#	git commit -m $(m);
#	git push;
	git status
	#INFO: no_ebin_commit ENDED SUCCESSFUL
with_ebin_commit:
	#INFO: with_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf inventory;
	rm -rf catalog application_dir deployment_specs catalog_specs;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
#	rm -rf ebin;
#	git add *;
	git status
	#INFO: with_ebin_commit ENDED SUCCESSFUL
build:
	#INFO: with_ebin_commit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf inventory;
	rm -rf catalog application_dir deployment_specs catalog_specs;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
	rm -rf _build*;
	rm -rf ebin;
	#INFO: build ENDED SUCCESSFUL
clean:
	#INFO: clean STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf inventory;
	rm -rf catalog application_dir deployment_specs catalog_specs;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf *~ */*~ */*/*~;
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	#INFO: clean ENDED SUCCESSFUL
eunit: 
	#INFO: eunit STARTED
	#INFO: Cleaning up to prepare build STARTED	 
	#INFO: Deleting crash reports
	rm -rf erl_cra* rebar3_crashreport_GLURK;
	#INFO: Deleting euinit test applications dirs
	rm -rf log resource_discovery etcd;
	rm -rf inventory;
	rm -rf catalog application_dir deployment_specs catalog_specs;
	rm -rf doc;
	rm -rf test_ebin;
	#INFO: Deleting tilde files and beams
	rm -rf src/*.beam src/*/*.beam;
	rm -rf test/*.beam test/*/*.beam;
	rm -rf *.beam;
	#INFO: Deleting files and dirs created during builds
	rm -rf _build;
	rm -rf ebin;
	rm -rf rebar.lock
	#INFO: Deleting files and dirs created during execution/runtime 
	rm -rf logs;
	rm -rf *_a;
	#INFO: Creating eunit test code using test_ebin dir;
	mkdir test_ebin;
	cp test/*.app test_ebin;
	#rm test/dependent_apps.erl;
	#cp /home/joq62/erlang/dev_support/dependent_apps.erl test;
	erlc -I include -I /home/joq62/erlang/include -o test_ebin test/*.erl;
	#INFO: Creating Common applications needed for testing
	#INFO: Compile application
	mkdir ebin;		
	rebar3 compile;	
	cp _build/default/lib/*/ebin/* ebin;
#	rm -rf _build*;
	#INFO: Starts the eunit testing .................
	erl -pa ebin -pa priv -pa test_ebin\
	 -pa /home/joq62/erlang/dev/log/ebin\
	 -pa /home/joq62/erlang/dev/resource_discovery/ebin\
	 -pa /home/joq62/erlang/dev/git_handler/ebin\
	 -pa /home/joq62/erlang/dev/catalog/ebin\
	 -pa /home/joq62/erlang/dev/deployment/ebin\
	 -sname controller_a\
	 -run $(m) start\
	 -setcookie a
