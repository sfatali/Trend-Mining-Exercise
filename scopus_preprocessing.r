grouping = function(text) {

text <- gsub("open source", "opensource", text)
text <- gsub("internet of things", "iot", text)
text <- gsub("internet  things", "iot", text)
text <- gsub("internet things", "iot", text)
text <- gsub("service oriented architecture", "soa", text)
text <- gsub("service oriented", "soa", text)
text <- gsub("service-oriented architecture", "soa", text)
text <- gsub("service-oriented", "soa", text)
text <- gsub("web services", "webservice", text)
text <- gsub("web service", "webservice", text)
text <- gsub("business process modelling", "businessprocessmodelling", text)
text <- gsub("workflows", "workflow", text)
text <- gsub("collaborative platform", "collaborativeplatform", text)
text <- gsub("case study", "casestudy", text)
text <- gsub("study methodology", "studymethodology", text)
text <- gsub("cloud computing", "cloud", text)
text <- gsub("containerization", "containers", text)
text <- gsub("containerized", "containers", text)
text <- gsub("containerize", "containers", text)
text <- gsub("containerizing", "containers", text)
text <- gsub("container", "containers", text)
text <- gsub("containerss", "containers", text)
text <- gsub("large scale", "largescale", text)
text <- gsub("multi-cloud", "cloud", text)
text <- gsub("big data", "bigdata", text)
text <- gsub("clouds", "cloud", text)
text <- gsub("machine learning", "machinelearning", text)
text <- gsub(" ml ", " machinelearning ", text)
text <- gsub("flexible solution", "flexibility", text)
text <- gsub("migrating", "migration", text)
text <- gsub("migrated", "migration", text)
text <- gsub("migrate", "migration", text)
text <- gsub("scalable", "scalability", text)
text <- gsub("reliable", "reliability", text)
text <- gsub("flexible", "flexibility", text)
text <- gsub("fast", "speed", text)
text <- gsub("quick", "speed", text)
text <- gsub("quickly", "speed", text)
text <- gsub("speedly", "speed", text)
text <- gsub("speedy", "speed", text)
text <- gsub("speeding", "speed", text)
text <- gsub("faster", "speed", text)
text <- gsub("rapid", "speed", text)
text <- gsub("operating systems", "operatingsystem", text)
text <- gsub("operating system", "operatingsystem", text)
text <- gsub("smart buildings", "smartbuildings", text)
text <- gsub("smart building", "smartbuildings", text)
text <- gsub("large-scale", "largescale", text)
text <- gsub("large scale", "largescale", text)
text <- gsub("orchestrate", "orchestration", text)
text <- gsub("orchestrating", "orchestration", text)
text <- gsub("virtualizing", "virtualization", text)
text <- gsub("virtual", "virtualization", text)
text <- gsub("virtualizationization", "virtualization", text)
text <- gsub("optimizing", "optimization", text)
text <- gsub("optimized", "optimization", text)
text <- gsub("optimize", "optimization", text)
text <- gsub("optimal", "optimization", text)
text <- gsub("digitalized", "digitalization", text)
text <- gsub("digitalize", "digitalization", text)
text <- gsub("digital", "digitalization", text)
text <- gsub("digitalizationization", "digitalization", text)
text <- gsub("communicating", "communication", text)
text <- gsub("communicated", "communication", text)
text <- gsub("communicate", "communication", text)
text <- gsub("communications", "communication", text)
text <- gsub("decomposing", "decomposition", text)
text <- gsub("decomposed", "decomposition", text)
text <- gsub("decompose", "decomposition", text)
text <- gsub("deploying", "deployment", text)
text <- gsub("deployed", "deployment", text)
text <- gsub("deployments", "deployment", text)
text <- gsub("deploy", "deployment", text)
text <- gsub("deploymentment", "deployment", text)
text <- gsub("agility", "agile", text)
text <- gsub("infratructures", "infratructure", text)
text <- gsub("architectural", "architecture", text)
text <- gsub("high level", "highlevel", text)
text <- gsub("low level", "lowlevel", text)
text <- gsub("configured", "configuration", text)
text <- gsub("configuring", "configuration", text)
text <- gsub("configure", "configuration", text)
text <- gsub("e-commerce", "ecommerce", text)
text <- gsub("evolving", "evolution", text)
text <- gsub("evolves", "evolution", text)
text <- gsub("evolve", "evolution", text)
text <- gsub("natural language processing", "nlp", text)
text <- gsub("language processing", "nlp", text)
text <- gsub("apis", "api", text)
text <- gsub("neural networks", "neuralnetworks", text)
text <- gsub("neural network", "neuralnetworks", text)
text <- gsub("collaborative", "collaboration", text)
text <- gsub("continuously", "continuous", text)
text <- gsub("component", "components", text)
text <- gsub("componentss", "components", text)
text <- gsub("resource", "resources", text)
text <- gsub("resourcess", "resources", text)
text <- gsub("platforms", "platform", text)
text <- gsub("technologies", "technology", text)
text <- gsub("challenge", "challenges", text)
text <- gsub("challengess", "challenges", text)
text <- gsub("contexts", "contex", text)
text <- gsub("complex", "complexity", text)
text <- gsub("complexityity", "complexity", text)

}
