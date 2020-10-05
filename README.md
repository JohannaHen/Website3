# Website3

[build] 
  publish = "public"
  command = "hugo"

[build.environment]
  HUGO_VERSION = "0.75.1"
  HUGO_ENABLEGITINFO = "true"

[context.production.environment]
  HUGO_ENV = "production"
  
[context.branch-deploy.environment]
  HUGO_VERSION = "0.75.1" 

[context.deploy-preview.environment]
  HUGO_VERSION = "0.75.1"
  