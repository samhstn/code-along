# The good parts of AWS

# Part 1: The Good Parts

## The default Heuristic

Making technical choices can be overwhelming. As developers, we have to make many choices from what seems like unlimited options.
Searching for the optimal tool for the job is always expensive. We cannot exhaustively test each option.

If the cost of acquiring new information is high,
and the consequence of deviating from a default choice is low,
then sticking with the default will likely be the optimal choice.

A default choice is any option that gives you high confidence that it will work - it simply needs to be a reliable option.

## DynamoDB

It's an immediately consistent, durable and scalable data store.

DynamoDB indexes come in two flavours: local and global.

Local indexes only advantage is that they're immediately consistent, but once you create a local index on a table,
they can no longer grow indefinitely, all records that share the same partition key need to fit in 10GB.
Once exhausted, all writes with that partition key fail, unless you're sure you won't exceed that limit, avoid local indexes.

Global indexes are eventually consistent - often hardly noticeable.
Global indexes don't constrain table size, but writes may need to be throttled as the internal queue-like system has a fixed size.
If not throttled, all operations on the main table start failing.

## S3

The first thing to consider using when storing any sort of data. It requires no capacity management.
It can be thought of a hash table in the cloud where the key is any string and the value is any data up to 5TB.

## EC2

EC2 allows you get a complete computer in the cloud in a matter of seconds, you only pay for the number of seconds
your instance is running.
EC2 has instance types optimized for CPU, memory, network, storage, etc.
On demand pricing is the most expensive, there are saving plan options, but spot instances are the cheapest where aws can take away your instance whenever it wants.

There are two important concepts for EC2 network security: the security group (individual firewalls for instances) and the VPC ACL (a network firewall).

## EC2 Auto Scaling

Sounds great in theory, but it's almost never useful except in very specific situations.
It's best to only use auto-scaling if EC2 costs are a real problem.

Auto Scaling can automatically replace an instance if it becomes unhealthy.
If you are using a load balancer, you can use the same health checks for both the load balancer and the Auto Scaling.

Auto Scaling has the ability to add or remove instances just by updating the desired capacity setting.

## Lambda

A code runner in the cloud.

Most suitable for small snippets of code that rarely change. It's good to think of them as part of the infrastructure rather than part of the application.

Some examples for how lambdas can extend existing AWS features:
+ S3 image resizing after upload.
+ ALBs come with an API to respond with a fixed response for a given route, but they can't respond with an image, lambdas allow your load balancer to do that.
+ CloudFront can't rewrite a request URL based on request cookies (useful for A/B testing), lambdas allow CloudFront to do that.
+ CloudWatch doesn't support regex-based alerting on application logs, but lambdas allow this.

There are lots of hidden limitations with lambdas:
+ cold start when a function is invoked after a period of inactivity.
+ there is a 250MB limit for your code bundle including dependencies.
+ the network bandwidth from Lambdas seems to be limited and unpredictable.
+ every Lambda invocation is stateless.

The rule of thumb for lambdas use case is:
If you have a small piece of code that will rarely need to be changed and that needs to run in response to something in your AWS account, lambdas are a good choice.
For everything else, a lot of caution is advised.
It's certainly not a substitute for EC2.

## ELB

ELB is a load balancer service, and comes in three variants: Application (ALB), Network (NLB), and Classic (the legacy option - not to be used for a new setup).

(A reverse proxy acts on behalf of the servers and a forward proxy acts on behalf of the clients).

ALBs are proper reverse proxies that sit between the internet and your application.
They handle every request to your application and forward every application response to the caller.
ALBs support sophisticated routing rules, redirects, responses from Lambda functions, authentication, sticky sessions.

NLBs behave like load balancers, but they work by routing network packets. Servers see the client as if it were connected to the client directly.

ALBs and NLBs don't validate certificates, but since load balancers run in a VPC, network packets only go to the hosts configured in the load balancer.

ALBs have 2 disadvantages: the proxy approach adds a few milliseconds to each request, and they may not scale quickly to handle a big burst of traffic
(it uses throttling and scaling when this happens).

## CloudFormation

Rule of thumb: let CloudFormation deal with AWS things that are either static or change very rarely;
VPC configurations, security groups, load balancers deployment pipelines, and IAM roles.
DynamoDB tables, Kinesis streams, Auto Scaling settings and sometimes S3 buckets are better managed elsewhere.
Route53 domain name registration, certificate creation and validation are too infrequently touched and too hard to automate that it doesn't make sense to automate.
Spending an unbounded amount of time scripting everything is not advisable.

## Route53

It lets you translate domain names to IP addresses. It integrates very well with ELB and DNS record creation can be automated with Cloudformation.

## SQS

A highly-durable queue in the cloud and a great choice for dispatching asynchronous work.
There is a lack of strict ordering and possibility of duplicates, but there is a FIFO (first in first out)
option which enables this, but has a throughput limit of 300 messages per second.

## Kinesis

A highly-durable linked list in the cloud. SQS can only have one costomer, while Kinesis can have many.
Once an SQS message gets consumed, it is deleted from the queue. But Kinesis records get added to a list in a stable order and any number
of consumers can read a copy of the stream. Whenever consumers read data out of Kinesis, they always get records in the same order.

Kinesis streams are a pure append-only data strucure and is removed when it exceeds the retention period of 24 hours by default.

Kinesis is not as easy to use as SQS. With SQS you don't worry about capacity or throttling limits,
a Kinesis stream is made up of slices of capacity called shards and it is up to you to figure out how many shards you need,
monitor shoard utilization, add shards and figure out the best way to route records to shards.

# Part 2: The Bootstrap Guide

We write our build logs to `../logs` as having the directory outside of the application directory will prevent logs from being deleted with every CodeDeploy deployment.

## Set up SSM for SSH access.

Once the managed policies are correctly configured for the instance IAM role.

Locally we should run:

```bash
curl "https://s3.amazonaws.com/session-manager-downloads/plugin/latest/mac/sessionmanager-bundle.zip" \
  -o "sessionmanager-bundle.zip"
unzip sessionmanager-bundle.zip
sudo ./sessionmanager-bundle/install -i /usr/local/sessionmanagerplugin -b /usr/local/bin/session-manager-plugin
```

We then need to find an instance to connect to, these can be found with:

```bash
aws ec2 describe-instances --query "Reservations[].Instances[?State.Name=='running'][].[Tags[?Key=='Name'],InstanceId] | [*][[0][0].Value,[1]]" | cat
```

We then start the session with:

```bash
aws ssm start-session --profile awsbootstrap --target <target-instance-id>

sh-4.2$ sudo su ec2-user
```
