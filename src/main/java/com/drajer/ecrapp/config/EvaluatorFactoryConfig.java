package com.drajer.ecrapp.config;

import ca.uhn.fhir.context.FhirContext;
import org.opencds.cqf.cql.evaluator.builder.*;
import org.opencds.cqf.cql.evaluator.builder.library.LibraryContentProviderFactory;
import org.opencds.cqf.cql.evaluator.expression.ExpressionEvaluator;
import org.opencds.cqf.cql.evaluator.library.CqlFhirParametersConverter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

import java.util.function.Supplier;

@Configuration
public class EvaluatorFactoryConfig {


    public static final FhirContext ctx = FhirContext.forR4();

    @Autowired
    CqlFhirParametersConverter cqlFhirParametersConverter;

    @Autowired
    LibraryContentProviderFactory libraryContentProviderFactory;

    @Autowired
    DataProviderFactory dataProviderFactory;

    @Autowired
    TerminologyProviderFactory terminologyProviderFactory;

    @Autowired
    EndpointConverter endpointConverter;

    @Autowired
    ModelResolverFactory fhirModelResolverFactory;

    @Autowired
    Supplier<CqlEvaluatorBuilder> cqlEvaluatorBuilderSupplier;

    @Bean(name = "myExpressionEvaluator")
    @Scope("prototype")
    public ExpressionEvaluator evaluatorInstance() {
        return new ExpressionEvaluator(
                ctx,
                cqlFhirParametersConverter,
                libraryContentProviderFactory,
                dataProviderFactory,
                terminologyProviderFactory,
                endpointConverter,
                fhirModelResolverFactory,
                cqlEvaluatorBuilderSupplier
        );
    }
}
