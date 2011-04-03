{application, erl_classifier,
 [{description, "Naive Bayes classifier"},
  {vsn, "0.1.0"},
  {modules, [ec_danish_stemmer,
             ec_document_processing,
             ec_feature_extraction,
             ec_frequency_distribution,
             ec_null_stemmer,
             ec_tokenizer
            ]},
  {registered, []},
%%  {applications, [kernel, sasl, stdlib, mnesia]},
  {applications, [kernel, stdlib, mnesia]},
  {mod, {erl_classifier_app, []}}
 ]}.

