# Database Patterns


### Transaction Management

```typescript
// TypeORM transactions
import { DataSource } from 'typeorm';

export class TransferService {
  constructor(private dataSource: DataSource) {}

  async transferMoney(fromAccountId: string, toAccountId: string, amount: number) {
    const queryRunner = this.dataSource.createQueryRunner();
    await queryRunner.connect();
    await queryRunner.startTransaction();

    try {
      // Deduct from sender
      await queryRunner.manager.decrement(
        Account,
        { id: fromAccountId },
        'balance',
        amount
      );

      // Add to receiver
      await queryRunner.manager.increment(
        Account,
        { id: toAccountId },
        'balance',
        amount
      );

      // Create transaction record
      const transaction = queryRunner.manager.create(Transaction, {
        fromAccountId,
        toAccountId,
        amount,
        status: 'completed'
      });
      await queryRunner.manager.save(transaction);

      await queryRunner.commitTransaction();
      return transaction;
    } catch (error) {
      await queryRunner.rollbackTransaction();
      throw error;
    } finally {
      await queryRunner.release();
    }
  }
}

// Prisma transactions
import { PrismaClient } from '@prisma/client';

const prisma = new PrismaClient();

async function transferMoney(fromId: string, toId: string, amount: number) {
  return await prisma.$transaction(async (tx) => {
    // Deduct from sender
    const sender = await tx.account.update({
      where: { id: fromId },
      data: { balance: { decrement: amount } }
    });

    if (sender.balance < 0) {
      throw new Error('Insufficient funds');
    }

    // Add to receiver
    await tx.account.update({
      where: { id: toId },
      data: { balance: { increment: amount } }
    });

    // Create transaction record
    return await tx.transaction.create({
      data: {
        fromAccountId: fromId,
        toAccountId: toId,
        amount,
        status: 'completed'
      }
    });
  });
}
```
