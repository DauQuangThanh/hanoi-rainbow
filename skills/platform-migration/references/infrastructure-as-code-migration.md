# Infrastructure as Code Migration


### Terraform State Migration

```bash
# Migrate state between backends
# From local to S3
terraform init \
  -migrate-state \
  -backend-config="bucket=my-terraform-state" \
  -backend-config="key=prod/terraform.tfstate" \
  -backend-config="region=us-east-1"

# From Terraform Cloud to S3
terraform login  # Authenticate to TFC
terraform init -migrate-state
# Follow prompts to migrate

# Verify state
terraform state list
```

### IaC Platform Migration

```markdown
CloudFormation → Terraform

1. Export Resources:
   - Use CloudFormer or AWS CLI
   - Document all resources
   - Note dependencies

2. Import to Terraform:
   # Generate Terraform config
   terraform import aws_instance.web i-1234567890abcdef0
   
   # Verify import
   terraform plan

3. Refactor:
   - Organize into modules
   - Add variables
   - Apply best practices

4. Validate:
   - Run terraform plan (should be no changes)
   - Test in non-prod first
   - Gradually adopt in production
```
