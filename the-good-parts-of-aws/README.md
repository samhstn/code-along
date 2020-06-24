# The good parts of AWS

## The default Heuristic

Making technical choices can be overwhelming. As developers, we have to make many choices from what seems like unlimited options.
Searching for the optimal tool for the job is always expensive. We cannot exhaustively test each option.
The cost of acquiring new information is high and the consequence of deviating from a default choice is low, sticking with the default will likely be the optimal choice.
A default choice is any option that gives you high confidence that it will work - it simply needs to be a reliable option.

## DynamoDB

It's immediately consitent, durable and scalable.
In general start with on-demand pricing. Then as prices increase, consider moving to provisioned capacity.

DynamoDB indexes come in two flabours: local and global.

Local indexes only advantage is that they're immediately consistent, but once you create a local index on a table,
they can no longer grow indefinitely, all records that share the same partition key need to fit in 10GB.
Once exhausted, all writes with that partition key fail, unless you're sure you won't exceed that limit, avoid local indexes.

Global indexes are eventually consistent - often hardly noticable.
Global indexes don't constrain table size, but writes may need to be throttled as the internal queue-like system has a fixed size.
If not throttled, all operations on the main table start failing.

## S3

The first thing to consider using when storing any sort of data. It requires no capacity management.
It can be thought of a hash table in the cloud where the key is any string and the value is any data up to 5TB.

## EC2

EC2 allows you get a complete computer in the cloud in a matter of seconds, you only pay for the number of seconds
your instance is running.
EC2 has instance types optimized ofr CPU, memory, network, storage, etc.
On demand pricing is the most expensive, there are saving plan optinos, but spot instances are the cheapest where aws can take away your instance whenever it wants.

There are two important concepts for EC2 network security: the security group (individual firewalls for instances) and the VPC ACL (a network firewall).

## EC2 Auto Scaling

Sounds great in theory, but it's almost never useful except in very specific situations.
It's best to only use autoscaling if EC2 costs are a real problem.

Auto Scaling can automatically replace an instance if it becomes unhealthly.
If you are using a load balancer, you can use the same health checks for both the load balancer and the Auto Scaling.

Auto Scaling has the ability to add or remove instances just by updating the desired capacity setting.

## Lambda

A simple code runner in the cloud.
Most suitable for small snippets of code that rarely change. It's good to think of them as part of the infrastructure rather than part of the application.
Some examples for how lambdas can extend existing AWS features:
+ S3 image resizing after upload.
+ ALBs come with an API to respond with a fixed response for a given route, but they can't respond with an image, lambdas allow you load balancer to do that.
+ CloudFront can't rewrite a request URL based on request cookies (useful for A/B testing), lambdas allow CloudFront to do that.
+ CloudWatch doesn't support regex-based alerting on application logs, but lambdas allow this.

There are lots of hidden limitations with lambdas:
+ cold start when a function is invoked after a period of inactivity.
+ there is a 250MB limit for your code bundle including dependencies.
+ the network bandwidth from Lambdas seems to be limited and unpredictable.
+ every Lambda invocation is stateless.

The rule of thumb for lambdas use case is:
If you have a small piece of code that will rarely need to be changed and that needs to run in response to something in your AWS account, lambdas are a good choice.
For everything else, alot of caution is advised.
It's certainly not a substitute for EC2.

## ELB

ELB is a load balancer service, and comes in three variants: Application (ALB), Network (NLB), and Classic (the legacy option - not to be used for a new setup).

(A reverse proxy acts on behalf of the servers and a forward proxy acts on behalf of the clients).

ALBs are proper reverse proxies that sit between the internet and your application.
They handle every request to your application and forward every application response to the caller.
ALBs support sophisticated routing rules, redirects, responses from Lambda functions, authentication, sticky sessions.

NLBs behave like load balancers, but they work by routing network packets...
