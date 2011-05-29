{application, erl_classifier,
 [{description, "Naive Bayes classifier"},
  {vsn, "0.1.0"},
  {modules, [	
  	    	ec_any_of,
		ec_any_of_sup,
		ec_classifier,
		ec_classifier_launcher_sup,
		ec_class_trainer,
		ec_coach,
		ec_configuration,
		ec_danish_stemmer,
		ec_document_processing,
		ec_event_classifier,
		ec_frequency_distribution,
		ec_handler_classifier,
		ec_stemming,
		ec_stopwords,
		ec_store,
		ec_swedish_stemmer,
		ec_term_manager,
		ec_tokenizer,
		ec_training_sup,
		erl_classifier_app,
		erl_classifier_sup
            ]},
  {registered, []},
  %%{applications, [kernel, sasl, stdlib, mnesia]},
  {applications, [kernel, stdlib, mnesia]},
  {env, [  %% Configuration tuples
  	   %% Language of the classifier
	   %% Valid values are: danish or swedish
  	   {language, danish},
	   %% Classes making up the classifier
	   %% List the class atoms in the list
  	   {classes, [f1, cykling, fodbold]}
	   %%{classes, [atletik,biler,boeger,cykling,digitalt,erhverv,f1,film,
	   %%	      fodbold,forbrug,haandbold,indland,klima,kultur,mad,musik,
 	   %%	      penge,politik,rejser,sport,sundhed,tennis,uddannelse,udland,
 	   %%	      videnskab]}
	] },
  %% mod is : { module, one-start argument}
  {mod, {erl_classifier_app, [] }}
 ]}.

