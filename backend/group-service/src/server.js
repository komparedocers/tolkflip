const express = require('express');
const cors = require('cors');
const { v4: uuidv4 } = require('uuid');
const winston = require('winston');
const promClient = require('prom-client');
const db = require('../../shared/models/database');

const app = express();

// Configure logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'group-service.log' })
  ]
});

// Prometheus metrics
const register = new promClient.Registry();
promClient.collectDefaultMetrics({ register });

const groupsCreatedCounter = new promClient.Counter({
  name: 'groups_created_total',
  help: 'Total number of groups created'
});

const groupMembersGauge = new promClient.Gauge({
  name: 'group_members_total',
  help: 'Total number of group members across all groups'
});

register.registerMetric(groupsCreatedCounter);
register.registerMetric(groupMembersGauge);

// Middleware
app.use(cors({ origin: process.env.ALLOWED_ORIGINS || '*' }));
app.use(express.json());

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', service: 'group-service' });
});

// Metrics endpoint
app.get('/metrics', async (req, res) => {
  res.set('Content-Type', register.contentType);
  res.end(await register.metrics());
});

// Create a new group
app.post('/api/groups', async (req, res) => {
  try {
    const { name, description, creatorId, memberIds, groupIcon } = req.body;

    if (!name || !creatorId) {
      return res.status(400).json({ error: 'Name and creator ID are required' });
    }

    const groupId = uuidv4();
    const createdAt = new Date();

    // Create group
    const groupData = {
      group_id: groupId,
      name,
      description: description || '',
      group_icon: groupIcon || '',
      created_by: creatorId,
      created_at: createdAt,
      updated_at: createdAt,
      member_count: 1,
      settings: {
        only_admins_can_send: false,
        only_admins_can_edit_info: true,
        only_admins_can_add_members: false,
        send_messages: true,
        edit_group_info: false,
        add_members: false
      }
    };

    await db.createGroup(groupData);

    // Add creator as admin
    await db.addGroupMember({
      group_id: groupId,
      user_id: creatorId,
      role: 'admin',
      joined_at: createdAt
    });

    // Add other members if specified
    if (memberIds && Array.isArray(memberIds)) {
      for (const memberId of memberIds) {
        if (memberId !== creatorId) {
          await db.addGroupMember({
            group_id: groupId,
            user_id: memberId,
            role: 'member',
            joined_at: createdAt
          });
        }
      }

      // Update member count
      await db.updateGroupMemberCount(groupId, memberIds.length + 1);
    }

    groupsCreatedCounter.inc();
    groupMembersGauge.inc(memberIds ? memberIds.length + 1 : 1);

    logger.info(`Group created: ${groupId} by ${creatorId}`);

    res.status(201).json({
      success: true,
      groupId,
      message: 'Group created successfully'
    });
  } catch (error) {
    logger.error('Error creating group:', error);
    res.status(500).json({ error: 'Failed to create group' });
  }
});

// Get group details
app.get('/api/groups/:groupId', async (req, res) => {
  try {
    const { groupId } = req.params;

    const group = await db.getGroupById(groupId);

    if (!group) {
      return res.status(404).json({ error: 'Group not found' });
    }

    res.json({ group });
  } catch (error) {
    logger.error('Error fetching group:', error);
    res.status(500).json({ error: 'Failed to fetch group' });
  }
});

// Update group info (name, description, icon)
app.put('/api/groups/:groupId', async (req, res) => {
  try {
    const { groupId } = req.params;
    const { userId, name, description, groupIcon } = req.body;

    if (!userId) {
      return res.status(400).json({ error: 'User ID is required' });
    }

    // Check if user is an admin or has permission
    const member = await db.getGroupMember(groupId, userId);

    if (!member) {
      return res.status(403).json({ error: 'You are not a member of this group' });
    }

    const group = await db.getGroupById(groupId);
    const settings = group.settings || {};

    // Check permissions
    if (settings.only_admins_can_edit_info && member.role !== 'admin') {
      return res.status(403).json({
        error: 'Only admins can edit group information'
      });
    }

    // Update group
    const updates = {};
    if (name) updates.name = name;
    if (description !== undefined) updates.description = description;
    if (groupIcon !== undefined) updates.group_icon = groupIcon;
    updates.updated_at = new Date();

    await db.updateGroup(groupId, updates);

    logger.info(`Group updated: ${groupId} by ${userId}`);

    res.json({ success: true, message: 'Group updated successfully' });
  } catch (error) {
    logger.error('Error updating group:', error);
    res.status(500).json({ error: 'Failed to update group' });
  }
});

// Add member to group
app.post('/api/groups/:groupId/members', async (req, res) => {
  try {
    const { groupId } = req.params;
    const { userId, newMemberId, role = 'member' } = req.body;

    if (!userId || !newMemberId) {
      return res.status(400).json({ error: 'User ID and new member ID are required' });
    }

    // Check if requesting user has permission
    const member = await db.getGroupMember(groupId, userId);

    if (!member) {
      return res.status(403).json({ error: 'You are not a member of this group' });
    }

    const group = await db.getGroupById(groupId);
    const settings = group.settings || {};

    if (settings.only_admins_can_add_members && member.role !== 'admin') {
      return res.status(403).json({ error: 'Only admins can add members' });
    }

    // Check if new member is already in the group
    const existingMember = await db.getGroupMember(groupId, newMemberId);

    if (existingMember) {
      return res.status(400).json({ error: 'User is already a member of this group' });
    }

    // Add new member
    await db.addGroupMember({
      group_id: groupId,
      user_id: newMemberId,
      role: role === 'admin' && member.role === 'admin' ? 'admin' : 'member',
      joined_at: new Date()
    });

    // Update member count
    await db.updateGroupMemberCount(groupId, group.member_count + 1);

    groupMembersGauge.inc();

    logger.info(`Member added to group: ${newMemberId} -> ${groupId}`);

    res.status(201).json({
      success: true,
      message: 'Member added successfully'
    });
  } catch (error) {
    logger.error('Error adding member:', error);
    res.status(500).json({ error: 'Failed to add member' });
  }
});

// Remove member from group
app.delete('/api/groups/:groupId/members/:memberId', async (req, res) => {
  try {
    const { groupId, memberId } = req.params;
    const { userId } = req.body;

    if (!userId) {
      return res.status(400).json({ error: 'User ID is required' });
    }

    // Check if requesting user has permission
    const member = await db.getGroupMember(groupId, userId);

    if (!member) {
      return res.status(403).json({ error: 'You are not a member of this group' });
    }

    // Users can remove themselves or admins can remove others
    if (userId !== memberId && member.role !== 'admin') {
      return res.status(403).json({ error: 'Only admins can remove other members' });
    }

    // Get the member to be removed
    const memberToRemove = await db.getGroupMember(groupId, memberId);

    if (!memberToRemove) {
      return res.status(404).json({ error: 'Member not found' });
    }

    // Remove member
    await db.removeGroupMember(groupId, memberId);

    // Update member count
    const group = await db.getGroupById(groupId);
    await db.updateGroupMemberCount(groupId, group.member_count - 1);

    groupMembersGauge.dec();

    logger.info(`Member removed from group: ${memberId} <- ${groupId}`);

    res.json({ success: true, message: 'Member removed successfully' });
  } catch (error) {
    logger.error('Error removing member:', error);
    res.status(500).json({ error: 'Failed to remove member' });
  }
});

// Get group members
app.get('/api/groups/:groupId/members', async (req, res) => {
  try {
    const { groupId } = req.params;
    const { limit = 100, offset = 0 } = req.query;

    const members = await db.getGroupMembers(groupId, parseInt(limit), parseInt(offset));

    res.json({ members, count: members.length });
  } catch (error) {
    logger.error('Error fetching group members:', error);
    res.status(500).json({ error: 'Failed to fetch group members' });
  }
});

// Update member role
app.put('/api/groups/:groupId/members/:memberId/role', async (req, res) => {
  try {
    const { groupId, memberId } = req.params;
    const { userId, newRole } = req.body;

    if (!userId || !newRole) {
      return res.status(400).json({ error: 'User ID and new role are required' });
    }

    if (!['admin', 'member'].includes(newRole)) {
      return res.status(400).json({ error: 'Invalid role. Must be "admin" or "member"' });
    }

    // Check if requesting user is an admin
    const member = await db.getGroupMember(groupId, userId);

    if (!member || member.role !== 'admin') {
      return res.status(403).json({ error: 'Only admins can change member roles' });
    }

    // Update role
    await db.updateGroupMemberRole(groupId, memberId, newRole);

    logger.info(`Member role updated: ${memberId} in ${groupId} -> ${newRole}`);

    res.json({ success: true, message: 'Member role updated successfully' });
  } catch (error) {
    logger.error('Error updating member role:', error);
    res.status(500).json({ error: 'Failed to update member role' });
  }
});

// Update group settings
app.put('/api/groups/:groupId/settings', async (req, res) => {
  try {
    const { groupId } = req.params;
    const { userId, settings } = req.body;

    if (!userId || !settings) {
      return res.status(400).json({ error: 'User ID and settings are required' });
    }

    // Check if user is an admin
    const member = await db.getGroupMember(groupId, userId);

    if (!member || member.role !== 'admin') {
      return res.status(403).json({ error: 'Only admins can update group settings' });
    }

    // Update settings
    await db.updateGroupSettings(groupId, settings);

    logger.info(`Group settings updated: ${groupId} by ${userId}`);

    res.json({ success: true, message: 'Group settings updated successfully' });
  } catch (error) {
    logger.error('Error updating group settings:', error);
    res.status(500).json({ error: 'Failed to update group settings' });
  }
});

// Get user's groups
app.get('/api/users/:userId/groups', async (req, res) => {
  try {
    const { userId } = req.params;
    const { limit = 50, offset = 0 } = req.query;

    const groups = await db.getUserGroups(userId, parseInt(limit), parseInt(offset));

    res.json({ groups, count: groups.length });
  } catch (error) {
    logger.error('Error fetching user groups:', error);
    res.status(500).json({ error: 'Failed to fetch user groups' });
  }
});

// Delete/leave group
app.delete('/api/groups/:groupId', async (req, res) => {
  try {
    const { groupId } = req.params;
    const { userId } = req.body;

    if (!userId) {
      return res.status(400).json({ error: 'User ID is required' });
    }

    const group = await db.getGroupById(groupId);

    if (!group) {
      return res.status(404).json({ error: 'Group not found' });
    }

    // Check if user is the creator/admin
    const member = await db.getGroupMember(groupId, userId);

    if (!member) {
      return res.status(403).json({ error: 'You are not a member of this group' });
    }

    if (group.created_by !== userId) {
      // User is leaving the group, not deleting it
      await db.removeGroupMember(groupId, userId);
      await db.updateGroupMemberCount(groupId, group.member_count - 1);

      logger.info(`User left group: ${userId} <- ${groupId}`);

      return res.json({ success: true, message: 'Left group successfully' });
    }

    // User is the creator, delete the entire group
    await db.deleteGroup(groupId);

    logger.info(`Group deleted: ${groupId} by ${userId}`);

    res.json({ success: true, message: 'Group deleted successfully' });
  } catch (error) {
    logger.error('Error deleting/leaving group:', error);
    res.status(500).json({ error: 'Failed to delete/leave group' });
  }
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received: closing HTTP server');
  await db.shutdown();
  process.exit(0);
});

const PORT = process.env.PORT || 3011;
app.listen(PORT, async () => {
  await db.connect();
  logger.info(`Group Service running on port ${PORT}`);
});

module.exports = app;
